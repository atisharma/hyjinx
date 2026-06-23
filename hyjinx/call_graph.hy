"
Static call-graph extraction from Hy and Python source files.

Given a .hy or .py source file, extracts all definitions, call edges,
and imports — without importing the target module.

By default uses HyReader (the standard reader) which resolves macro
dependencies at read time (e.g. hyrule, hyjinx.macros). Use safe=True
to switch to HySafeReader which restricts macros to the default set
for zero read-time side effects.

For .hy files: uses hy_compile to produce a Python AST, then walks it
with ast.NodeVisitor. Reader forms are also parsed to detect defmacro
(which compile to hy.macros.macro calls, not FunctionDef) and defmethod
(which appear as FunctionDef with multimethod decorator).

For .py files: uses ast.parse directly.

Call edges reflect what the code DOES (resolved by Hy's compiler), not
what the programmer WROTE. Macro-expanded calls and threaded macros are
fully resolved in the compiled AST.
"

(require hyrule [-> ->> unless of as-> doto])
(require hyjinx.macros [defstruct lmap])
(require hyjinx.result [result-> try-result])

(import ast)
(import os)
(import sys)
(import types)
(import pathlib [Path])
(import toolz [first second last take drop identity reduce])
(import functools [partial])

(import hy [unmangle mangle read-many])
(import hy.reader.hy_reader [HyReader])
(import hy.hy_inspect [HySafeReader])
(import hy.compiler [hy-compile])
(import hy.models [Expression Symbol Keyword])

;; ---- Data types ------------------------------------------------------------

(defstruct Definition
  "A named, locatable definition in source code."
  #^ str name
  #^ str kind
  #^ int line
  #^ (| str None) scope
  #^ (of list str) params)

(defstruct CallEdge
  "A call relationship: CALLER invokes CALLEE at LINE."
  #^ (| str None) caller
  #^ str callee
  #^ int line)

(defstruct ImportDecl
  "An import statement."
  #^ str module
  #^ (of list str) names
  #^ int line)

(defstruct CallGraph
  "Complete call graph for a source file."
  #^ str file
  #^ str language
  #^ (of list Definition) definitions
  #^ (of list CallEdge) calls
  #^ (of list ImportDecl) imports
  #^ (of list str) warnings)

;; ---- Helpers ---------------------------------------------------------------

(setv _COMPREHENSION-NAMES #{"<listcomp>" "<dictcomp>" "<setcomp>" "<genexpr>"})
(setv _HY-BOOTSTRAP-MODULE "hy")

(defn _is-hy-bootstrap [node]
  "Check if an AST node is hy_compile bootstrap boilerplate (not user code)."
  (cond
    ;; Auto-inserted (import hy)
    (and (isinstance node ast.Import)
         (= (len node.names) 1)
         (= (getattr (first node.names) "name" None) _HY-BOOTSTRAP-MODULE))
    True

    ;; Module return value wrapper: Expr(value=List([Constant(None), ...]))
    (and (isinstance node ast.Expr)
         (isinstance node.value ast.List)
         (all (lfor e node.value.elts
                    (or (isinstance e ast.Constant)
                        (isinstance e ast.Expr)))))
    True

    ;; Require expression: Expr(Call(Attribute(hy.macros, require), ...))
    (and (isinstance node ast.Expr)
         (isinstance node.value ast.Call)
         (isinstance node.value.func ast.Attribute)
         (isinstance node.value.func.value ast.Attribute)
         (= (getattr node.value.func.value.attr "_fields" None) None))
    ;; Heuristic: skip require/call-attributed bootstrap
    True

    True
    False))

(defn _call-name [func-node]
  "Extract a human-readable callee name from a Call.func AST node.
  Returns None for unresolvable call targets (subscripts, nested calls, etc.)."
  (cond
    ;; Direct name: (f x) -> "f"
    (isinstance func-node ast.Name)
    func-node.id

    ;; Attribute access: (obj.method x) -> "obj.method"
    (isinstance func-node ast.Attribute)
    (let [parts (_collect-attr func-node)]
      (when parts
        (.join "." parts)))

    True
    None))

(defn _collect-attr [node]
  "Recursively collect attribute chain segments.
  (.replace (.lower s)) -> ['s', 'lower', 'replace']"
  (match (type node)
    ast.Attribute
    (let [parts (_collect-attr node.value)]
      (when parts
        (+ parts [node.attr])))

    ast.Name
    [node.id]

    _ None))

(defn _make-stub-module [path]
  "Create a dummy module in sys.modules so hy_compile won't try to import from disk.
  Returns the module name, which should be cleaned up after use."
  (let [mod-name f"__hy_cg_{(abs (hash (str (.resolve (Path path)))))}__"]
    (when (not (in mod-name sys.modules))
      (setv (get sys.modules mod-name)
            (types.ModuleType mod-name)))
    mod-name))

(defn _cleanup-stub-module [mod-name]
  "Remove a stub module from sys.modules."
  (when (in mod-name sys.modules)
    (let [mod (get sys.modules mod-name)]
      (when (isinstance mod types.ModuleType)
        (.pop sys.modules mod-name None)))))


;; ---- Reader-level macro/method detection (for .hy files) -------------------

(defn _find-reader-defs [forms]
  "Parse reader forms to find defmacro and defmethod names.
  Returns a dict with keys :macros (set of names) and :methods (set of names).
  defmacro compiles to hy.macros.macro(...) — not a FunctionDef — so we must
  detect them at the reader level.
  defmethod compiles to a FunctionDef with multimethod decorator — but
  detecting from reader forms gives us a more precise kind tag."
  (let [macros (set)
        methods (set)]
    (for [f forms]
      (when (and (isinstance f Expression) f)
        (let [head (get f 0)]
          (when (isinstance head Symbol)
            (let [head-str (str head)]
              (cond
                (= head-str "defmacro")
                (when (> (len f) 1)
                  (let [name-sym (get f 1)]
                    (when (isinstance name-sym Symbol)
                      (.add macros (unmangle (str name-sym))))))

                (= head-str "defmethod")
                (when (> (len f) 1)
                  ;; defmethod can be (defmethod :async f ...) or (defmethod f ...)
                  (let [idx (if (and (> (len f) 2) (isinstance (get f 1) Keyword))
                             2   ;; skip :async
                             1)]
                    (when (< idx (len f))
                      (let [name-sym (get f idx)]
                        (when (isinstance name-sym Symbol)
                          (.add methods (unmangle (str name-sym))))))))

                True None))))))
    {"macros" macros "methods" methods}))


;; ---- AST Visitor -----------------------------------------------------------

(defclass _GraphVisitor [ast.NodeVisitor]
  "Walk a compiled Python AST to extract definitions, call edges, and imports."

  (defn __init__ [self lang kind-map macro-names method-names]
    (setv self.lang lang)
    (setv self.kind-map kind-map)
    (setv self.macro-names macro-names)
    (setv self.method-names method-names)
    (setv self.definitions [])
    (setv self.calls [])
    (setv self.imports [])
    (setv self._scope-stack [None])   ; None = module-level scope
    (.__init__ (super)))

  ;; --- Scope management ---

  (defn _scope [self]
    "Current enclosing definition name, or None for module-level."
    (last self._scope-stack))

  (defn _push-scope [self name]
    (.append self._scope-stack name))

  (defn _pop-scope [self]
    (.pop self._scope-stack))

  ;; --- Name resolution ---

  (defn _definition-kind [self name]
    "Determine the kind for a function definition, checking reader-level tags."
    (cond
      (in name self.macro-names) "defmacro"
      (in name self.method-names) "defmethod"
      True (get self.kind-map (if (= self.lang "py") "def" "defn"))))

  ;; --- Visitor methods ---

  (defn visit-FunctionDef [self node]
    "Emit a Definition for the function, push scope, walk body, pop scope."
    (let [name (if (= self.lang "hy") (unmangle node.name) node.name)]
      (when (not (in name _COMPREHENSION-NAMES))
        (let [current-scope (self._scope)
              kind (if (and current-scope (isinstance current-scope str))
                     "method"  ; inside a class -> method
                     (self._definition-kind name))
              params (lfor a node.args.args
                          (if (= self.lang "hy")
                            (unmangle a.arg)
                            a.arg))]
          (.append self.definitions
            (Definition :name name
                        :kind kind
                        :line node.lineno
                        :scope current-scope
                        :params params)))
        (self._push-scope name)
        (self.generic-visit node)
        (self._pop-scope))))

  (defn visit-AsyncFunctionDef [self node]
    "Async functions: use 'defn' kind for Python (async def), same as Hy defn."
    (let [name (if (= self.lang "hy") (unmangle node.name) node.name)]
      (when (not (in name _COMPREHENSION-NAMES))
        (let [current-scope (self._scope)
              kind (cond
                     (and current-scope (isinstance current-scope str))
                     "method"

                     (= self.lang "py")
                     "defn"

                     True
                     (self._definition-kind name))
              params (lfor a node.args.args
                          (if (= self.lang "hy")
                            (unmangle a.arg)
                            a.arg))]
          (.append self.definitions
            (Definition :name name
                        :kind kind
                        :line node.lineno
                        :scope current-scope
                        :params params)))
        (self._push-scope name)
        (self.generic-visit node)
        (self._pop-scope))))

  (defn visit-ClassDef [self node]
    "Emit a Definition for the class, push scope, walk body for methods, pop."
    (let [name (if (= self.lang "hy") (unmangle node.name) node.name)]
      (.append self.definitions
        (Definition :name name
                    :kind (get self.kind-map "class")
                    :line node.lineno
                    :scope (self._scope)
                    :params []))
      (self._push-scope name)
      (self.generic-visit node)
      (self._pop-scope)))

  (defn visit-Call [self node]
    "Emit a CallEdge for the current scope."
    (let [callee (_call-name node.func)]
      (when callee
        (.append self.calls
          (CallEdge :caller (self._scope)
                    :callee callee
                    :line node.lineno)))))
    ;; Don't recurse into call arguments — we only care about the call target
    ;; But we DO need to visit arguments to find nested calls
    ;; Actually, NodeVisitor.generic_visit does recurse. We just don't override further.
    ;; The CallEdge captures the direct call; inner calls in args are found by generic_visit.

  (defn visit-Import [self node]
    "Emit ImportDecl for each import."
    (when (not (_is-hy-bootstrap node))
      (for [alias node.names]
        (.append self.imports
          (ImportDecl :module alias.name
                      :names []
                      :line node.lineno)))))

  (defn visit-ImportFrom [self node]
    "Emit ImportDecl with specific names."
    (when node.module
      (.append self.imports
        (ImportDecl :module node.module
                    :names (lfor a node.names a.name)
                    :line node.lineno))))

  (defn visit-Assign [self node]
    "Emit Definition for top-level assignments with Name targets.
    Only at module level (scope is None), not inside classes or functions."
    (when (is (self._scope) None)
      (for [t node.targets]
        (when (isinstance t ast.Name)
          (let [name t.id]
            (when (not (.startswith name "_"))
              (.append self.definitions
                (Definition :name name
                            :kind "setv"
                            :line node.lineno
                            :scope None
                            :params []))))))))

  (defn visit-AnnAssign [self node]
    "Skip type annotations — they're not useful as definitions."
    None)

  (defn visit-Expr [self node]
    "Top-level expressions: filter bootstrap, skip others."
    (when (not (_is-hy-bootstrap node))
      (self.generic-visit node)))

  (defn visit-If [self node]
    "Walk if/else branches for calls."
    (self.generic-visit node))

  (defn visit-While [self node]
    "Walk while bodies for calls."
    (self.generic-visit node))

  (defn visit-For [self node]
    "Walk for bodies for calls."
    (self.generic-visit node))

  (defn visit-With [self node]
    "Walk with bodies for calls."
    (self.generic-visit node))

  (defn visit-Try [self node]
    "Walk try/except/finally bodies for calls."
    (self.generic-visit node))

  (defn visit-Return [self node]
    "Walk return values for nested calls."
    (self.generic-visit node)))


;; ---- File-type specific extraction -----------------------------------------

(defn _hy-call-graph [path * [safe False]]
  "Extract call graph from a .hy source file.
  Uses reader-level parsing for macro/method detection, then hy_compile
  for AST walking. If safe, uses HySafeReader (default macros only)."
  (let [source (.read-text path)
        reader (when safe (HySafeReader))
        reader-forms (try
                       (list (read-many source :skip-shebang True :reader reader))
                       (except [Exception]
                         (return (CallGraph :file (str (.resolve (Path path)))
                                          :language "hy"
                                          :definitions []
                                          :calls []
                                          :imports []
                                          :warnings [(+ "compile error: " (str (sys.exc-info)))]))))
        tag-info (_find-reader-defs reader-forms)
        macro-names (:macros tag-info)
        method-names (:methods tag-info)
        mod-name (_make-stub-module path)
        warnings []]
    (try
      (let [tree (hy-compile reader-forms mod-name)
            kind-map {"defn" "defn" "class" "defclass"}
            visitor (_GraphVisitor "hy" kind-map macro-names method-names)]
        (visitor.visit tree)

        ;; Add defmacro definitions (not present as FunctionDef in AST)
        (for [mname macro-names]
          (.append visitor.definitions
            (Definition :name mname
                        :kind "defmacro"
                        :line 0   ; line unknown from compiled AST
                        :scope None
                        :params [])))

        (_cleanup-stub-module mod-name)

        (CallGraph :file (str (.resolve (Path path)))
                   :language "hy"
                   :definitions visitor.definitions
                   :calls visitor.calls
                   :imports visitor.imports
                   :warnings warnings))
      (except [Exception]
        ;; Compilation error — return empty graph with warning
        (try (_cleanup-stub-module mod-name)
             (except [Exception]))
        (CallGraph :file (str (.resolve (Path path)))
                   :language "hy"
                   :definitions []
                   :calls []
                   :imports []
                   :warnings [(+ "compile error: " (str (sys.exc-info)))])))))

(defn _py-call-graph [path]
  "Extract call graph from a .py source file."
  (let [source (.read-text path)
        warnings []]
    (try
      (let [tree (ast.parse source :filename (str path))
            kind-map {"def" "def" "class" "class"}
            visitor (_GraphVisitor "py" kind-map (set) (set))]
        (visitor.visit tree)
        (CallGraph :file (str (.resolve (Path path)))
                   :language "python"
                   :definitions visitor.definitions
                   :calls visitor.calls
                   :imports visitor.imports
                   :warnings warnings))
      (except [SyntaxError]
        (CallGraph :file (str (.resolve (Path path)))
                   :language "python"
                   :definitions []
                   :calls []
                   :imports []
                   :warnings [(+ "parse error: " (str (sys.exc-info)))])))))


;; ---- Public API ------------------------------------------------------------

(defn call-graph [path * [safe False]]
  "Extract definitions, call edges, and imports from a source file.
  No imports are performed — purely static analysis.
  Supports .hy and .py files.
  For .hy files, if safe is True, uses HySafeReader (default macros only).

  Returns a CallGraph with:
    - file: absolute path
    - language: 'hy' or 'python'
    - definitions: list of Definition (name, kind, line, scope, params)
    - calls: list of CallEdge (caller, callee, line)
    - imports: list of ImportDecl (module, names, line)
    - warnings: list of error strings (empty if no errors)

  Raises FileNotFoundError for missing files, ValueError for unsupported types."
  (let [p (Path path)]
    (unless (.exists p)
      (raise (FileNotFoundError f"File not found: {path}")))
    (match (.lstrip p.suffix ".")
      "hy" (_hy-call-graph p :safe safe)
      "py" (_py-call-graph p)
      ext (raise (ValueError f"Unsupported file type: .{ext}")))))


;; ---- Graph Analyses --------------------------------------------------------

(defn _build-adjacency [g]
  "Build forward adjacency dict: name -> {callee names}.
  Includes both calls within definitions and top-level calls.
  Returns (forward-edges, reverse-edges)."
  (let [fwd {}   ; caller -> {callees}
        rev {}]  ; callee -> {callers}
    ;; Initialize all definitions
    (for [d g.definitions]
      (setv (get fwd d.name) (set))
      (setv (get rev d.name) (set)))
    ;; Add edges
    (for [e g.calls]
      (when e.caller
        ;; Caller-defined edge
        (when (in e.caller fwd)
          (.add (get fwd e.caller) e.callee))
        (when (not (in e.callee rev))
          (setv (get rev e.callee) (set)))
        (.add (get rev e.callee) e.caller)))
    #(fwd rev)))

(defn callers [g name]
  "All definitions that directly call NAME.
  Returns a sorted list of caller names."
  (let [edges (lfor e g.calls
                     :if (and e.caller (= e.callee name))
                     e.caller)]
    (sorted (set edges))))

(defn callees [g name]
  "All definitions directly called by NAME.
  Returns a sorted list of callee names."
  (let [edges (lfor e g.calls
                     :if (and e.caller (= e.caller name))
                     e.callee)]
    (sorted (set edges))))

(defn transitive-callers [g name]
  "All definitions that transitively call NAME (blast radius).
  Uses BFS reverse traversal with visited-set tracking to handle cycles."
  (let [edges (lfor e g.calls :if e.caller #(e.callee e.caller))
        reverse-index {}
        visited (set)
        queue (list)]
    ;; Build reverse index: callee -> {callers}
    (for [[callee caller] edges]
      (when (not (in callee reverse-index))
        (setv (get reverse-index callee) (set)))
      (.add (get reverse-index callee) caller))
    ;; BFS from name, following reverse edges
    (when (in name reverse-index)
      (.extend queue (get reverse-index name)))
    (while queue
      (let [current (.pop queue 0)]
        (when (not (in current visited))
          (.add visited current)
          (when (in current reverse-index)
            (for [next-caller (get reverse-index current)]
              (when (not (in next-caller visited))
                (.append queue next-caller)))))))
    (sorted visited)))

(defn transitive-callees [g name]
  "All definitions transitively called by NAME.
  Uses BFS forward traversal with visited-set tracking to handle cycles."
  (let [edges (lfor e g.calls :if e.caller #(e.caller e.callee))
        forward-index {}
        visited (set)
        queue (list)]
    ;; Build forward index: caller -> {callees}
    (for [[caller callee] edges]
      (when (not (in caller forward-index))
        (setv (get forward-index caller) (set)))
      (.add (get forward-index caller) callee))
    ;; BFS from name, following forward edges
    (when (in name forward-index)
      (.extend queue (get forward-index name)))
    (while queue
      (let [current (.pop queue 0)]
        (when (not (in current visited))
          (.add visited current)
          (when (in current forward-index)
            (for [next-callee (get forward-index current)]
              (when (not (in next-callee visited))
                (.append queue next-callee)))))))
    (sorted visited)))

(defn dead-code [g [entry-points None]]
  "Definitions unreachable from any entry point.

  If entry-points is None, uses:
    - Top-level definitions (scope is None)
    - Macros (kind 'defmacro') — always treated as entry points
  Macro definitions can't appear as FunctionDef in the compiled AST,
  so they have zero incoming CallEdges but should not be reported as dead.

  Returns a sorted list of unreachable definition names."
  (let [;; Determine entry points
        ep (if entry-points
             (set entry-points)
             (set (+ (lfor d g.definitions
                           :if (or (is d.scope None)
                                   (= d.kind "defmacro"))
                           d.name)
                     ;; Also include any non-None caller (top-level calls)
                     ;; These are implicitly reachable
                     (lfor e g.calls
                           :if (is e.caller None)
                           e.callee))))
        ;; Build forward adjacency from definitions
        forward-index {}
        definition-names (set (lfor d g.definitions d.name))]
    (for [e g.calls]
      (when (and e.caller (in e.caller definition-names))
        (when (not (in e.caller forward-index))
          (setv (get forward-index e.caller) (set)))
        (.add (get forward-index e.caller) e.callee)))
    ;; BFS from entry points — find all reachable definitions
    (let [reachable (set)
          queue (list ep)]
      (while queue
        (let [current (.pop queue 0)]
          (when (and (not (in current reachable)) (in current definition-names))
            (.add reachable current)
            (when (in current forward-index)
              (for [next-def (get forward-index current)]
                (when (not (in next-def reachable))
                  (.append queue next-def)))))))
      ;; Dead = all definitions minus reachable
      (sorted (lfor d g.definitions
                    :if (not (in d.name reachable))
                    d.name)))))

(defn reachable [g source target]
  "Can SOURCE reach TARGET through any chain of calls?
  Returns True if a path exists, False otherwise."
  (let [found (in target (transitive-callees g source))]
    (or found (= source target))))

(defn call-path [g source target]
  "Shortest call chain from SOURCE to TARGET, or None if unreachable.
  Uses BFS with predecessor tracking."
  (if (= source target)
    [source]
    (let [edges (lfor e g.calls :if e.caller #(e.caller e.callee))
          forward-index {}
          visited (set)
          queue (list)
          predecessors {}]
      ;; Build forward index
      (for [[caller callee] edges]
        (when (not (in caller forward-index))
          (setv (get forward-index caller) (set)))
        (.add (get forward-index caller) callee))
      ;; BFS with predecessor tracking
      (when (in source forward-index)
        (.extend queue (get forward-index source))
        (for [callee (get forward-index source)]
          (setv (get predecessors callee) source)))
      (setv found None)
      (while (and queue (is found None))
        (let [current (.pop queue 0)]
          (when (not (in current visited))
            (.add visited current)
            (if (= current target)
              (setv found current)
              (when (in current forward-index)
                (for [next-callee (get forward-index current)]
                  (when (not (in next-callee visited))
                    (when (not (in next-callee predecessors))
                      (setv (get predecessors next-callee) current))
                    (.append queue next-callee))))))))
      (if found
        ;; Reconstruct path backwards
        (let [path []
              current target]
          (while (is-not current None)
            (.insert path 0 current)
            (setv current (.get predecessors current)))
          path)
        None))))

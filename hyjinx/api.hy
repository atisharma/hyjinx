"
Static API surface inspection — no imports, no side effects.

Parses Hy and Python source files to extract public definitions
(functions, classes, macros, top-level bindings) without importing
the module. Uses hy.reader.read_many for .hy files and ast.parse
for .py files.

This avoids triggering top-level code, DB connections, GPU init, etc.
"

(require hyrule [-> ->>])

(import hy [mangle unmangle])
(import hy.models [Expression List Symbol Keyword])
(import hy.reader [read-many])

(import ast)
(import pathlib [Path])
(import sys)
(import toolz [first last])
(import hyrule [inc])


;; * Internal: Hy form extraction

(defn _extract-params [params-form]
  "Extract parameter names from a Hy parameter list form."
  (let [names []]
    (for [p params-form]
      (cond
        (isinstance p Symbol)
        (let [s (str p)]
          (.append names
            (cond
              ;; These branches are currently dead code: the Hy reader
              ;; expands #* -> (unpack-iterable ...) and #** -> (unpack-mapping ...)
              ;; before we see them. Kept as defensive fallback.
              (= s "#*")  "*args"
              (= s "#**") "**kwargs"
              True       (unmangle s))))

        (and (isinstance p List) p (isinstance (first p) Symbol))
        (.append names (unmangle (str (first p))))

        (isinstance p Expression)
        ;; Handle (unpack-iterable args) from #* and (unpack-mapping kwargs) from #**
        (when (>= (len p) 2)
          (let [head (get p 0)]
            (when (and (isinstance head Symbol) (in (str head) ["unpack-iterable" "unpack-mapping"]))
              (let [op (str head)
                    inner (get p 1)]
                (.append names
                  (cond
                    (= op "unpack-iterable") (if (isinstance inner Symbol) (unmangle (str inner)) "*args")
                    (= op "unpack-mapping")   (if (isinstance inner Symbol) (unmangle (str inner)) "**kwargs")
                    True                       "??"))))))

        True
        None))
    names))

(defn _parse-defn [form]
  "Parse a defn/defmethod form, handling :async, decorators."
  (let [head (get form 0)
        kind (if (= (str head) "defmethod") "defmethod" "defn")
        idx 1]
    ;; skip :async keyword
    (when (and (< idx (len form))
               (isinstance (get form idx) Keyword))
      (setv idx (inc idx)))
    ;; skip decorator list
    (when (and (< idx (len form))
               (isinstance (get form idx) List))
      (setv idx (inc idx)))
    (when (>= (inc idx) (len form))
      (return None))
    (let [name-sym (get form idx)
          params-form (get form (inc idx))]
      (when (not (isinstance name-sym Symbol))
        (return None))
      {"kind" kind
       "name" (unmangle (str name-sym))
       "params" (if (isinstance params-form List)
                    (_extract-params params-form)
                    [])
       "line" (or form.start-line 1)})))

(defn _parse-defmacro [form]
  "Parse a defmacro form."
  (when (< (len form) 3)
    (return None))
  (let [name-sym (get form 1)
        params-form (get form 2)]
    (when (not (isinstance name-sym Symbol))
      (return None))
    {"kind" "defmacro"
     "name" (unmangle (str name-sym))
     "params" (if (isinstance params-form List)
                  (_extract-params params-form)
                  [])
     "line" (or form.start-line 1)}))

(defn _parse-setv [form]
  "Parse a setv form. May produce multiple bindings."
  (let [results []]
    (for [i (range 1 (- (len form) 1) 2)]
      (let [name-form (get form i)]
        (when (isinstance name-form Symbol)
          (.append results {"kind" "setv"
                            "name" (unmangle (str name-form))
                            "params" []
                            "line" (or form.start-line 1)}))))
    results))

(defn _parse-hy-form [form]
  "Extract a definition from a single top-level Hy form.
  Returns a dict, a list of dicts (from setv), or None."
  (when (or (not (isinstance form Expression)) (not form))
    (return None))
  (let [head (get form 0)]
    (when (not (isinstance head Symbol))
      (return None))
    (let [head-str (str head)]
      (cond
        (in head-str ["defn" "defn/a" "defmethod"])
        (_parse-defn form)

        (= head-str "defmacro")
        (_parse-defmacro form)

        (= head-str "defclass")
        (when (> (len form) 1)
          (let [name-sym (get form 1)]
            (when (isinstance name-sym Symbol)
              {"kind" "defclass"
               "name" (unmangle (str name-sym))
               "params" []
               "line" (or form.start-line 1)})))

        (= head-str "setv")
        (_parse-setv form)

        True
        None))))


;; * Internal: Python AST extraction

(defn _parse-py-def [node]
  "Extract a Python function definition from an AST node."
  (let [params (lfor a node.args.args a.arg)]
    {"kind" (if (isinstance node ast.AsyncFunctionDef) "defn" "def")
     "name" node.name
     "params" params
     "line" node.lineno}))

(defn _parse-py-tree [tree]
  "Walk top-level Python AST nodes and extract definitions."
  (let [results []]
    (for [node (ast.iter-child-nodes tree)]
      (cond
        (isinstance node (| ast.FunctionDef ast.AsyncFunctionDef))
        (.append results (_parse-py-def node))

        (isinstance node ast.ClassDef)
        (.append results {"kind" "class"
                          "name" node.name
                          "params" []
                          "line" node.lineno})

        (isinstance node ast.Assign)
        (for [t node.targets]
          (when (isinstance t ast.Name)
            (when (not (.startswith t.id "_"))
              (.append results {"kind" "setv"
                                "name" t.id
                                "params" []
                                "line" node.lineno}))))

        True
        None))
    results))


;; * Public API

(defn api-surface [path]
  "Return a list of definition dicts for a source file.
  Each dict has keys: kind, name, params, line.
  No imports are performed — purely static analysis."
  (let [p (Path path)
        source (.read-text p)]
    (cond
      (.endswith p.suffix ".hy")
      (let [forms (list (read-many source :skip-shebang True))
            results []]
        (for [f forms]
          (let [defn (_parse-hy-form f)]
            (when defn
              (if (isinstance defn list)
                (.extend results defn)
                (.append results defn)))))
        results)

      (.endswith p.suffix ".py")
      (_parse-py-tree (ast.parse source :filename (str p)))

      True
      (raise (ValueError f"Unsupported file type: {p.suffix}")))))

(defn _find-spec-fallback [dotted-name]
  "Fallback module resolver using importlib.util.find-spec.
  May trigger parent package imports — used only when sys.path walk fails."
  (import importlib.util [find-spec])
  (try
    (let [spec (find-spec dotted-name)]
      (when (and spec spec.origin)
        (let [origin spec.origin
              hy-path (.with-suffix (Path (.replace origin "__pycache__" "")) ".hy")]
          (if (.exists hy-path)
            hy-path
            (when (.exists (Path origin))
              (Path origin))))))
    (except [ImportError])
    (except [ModuleNotFoundError])
    (except [ValueError])))

(defn resolve-module-path [dotted-name]
  "Resolve a dotted module name to a source file path, without importing.
  Walks sys.path manually to avoid triggering module side effects.
  Falls back to importlib.util.find-spec for editable installs (which
  may trigger parent package imports).
  Returns a Path or None."
  (let [parts (.split dotted-name ".")
        candidates []]
    ;; First try: walk sys.path without importing anything
    (for [base sys.path]
      (when (and base (not (.startswith base "_")))
        (let [package-dir (Path base)]
          (when (.is-dir package-dir)
            ;; Walk into subdirectories for nested modules
            (for [p (cut parts 0 -1)]
              (setv package-dir (/ package-dir p)))
            (when (.is-dir package-dir)
              (let [base-name (last parts)]
                ;; .hy first (prefer Hy source)
                (let [hy-file (/ package-dir f"{base-name}.hy")]
                  (when (.exists hy-file)
                    (.append candidates hy-file)))
                ;; .py
                (let [py-file (/ package-dir f"{base-name}.py")]
                  (when (.exists py-file)
                    (.append candidates py-file)))
                ;; package __init__
                (let [init-dir (/ package-dir base-name)]
                  (when (.is-dir init-dir)
                    (let [init-hy (/ init-dir "__init__.hy")]
                      (when (.exists init-hy)
                        (.append candidates init-hy)))
                    (let [init-py (/ init-dir "__init__.py")]
                      (when (.exists init-py)
                        (.append candidates init-py)))))))))))
    (if candidates
      (first candidates)
      (_find-spec-fallback dotted-name))))

(defn format-surface [defs * [show-params True] [show-line False]]
  "Format a list of definition dicts as a readable table."
  (let [kind-tags {"defn" "defn"
                   "defmethod" "meth"
                   "def" "def "
                   "defclass" "cls "
                   "class" "cls "
                   "defmacro" "mac "
                   "setv" "var "}
        lines []]
    (for [d defs]
      (let [kind (:kind d)
            name (:name d)
            params (:params d)
            line (:line d)
            tag (.get kind-tags kind "    ")
            joined (if (and show-params params)
                      (.join ", " params)
                      "")
            param-str (if joined f"({joined})" "")
            line-str (if show-line f"  :{line}" "")]
        (.append lines f"  {tag} {name}{param-str}{line-str}")))
    (.join "\n" lines)))

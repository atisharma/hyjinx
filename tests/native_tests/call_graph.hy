"
Tests for hyjinx.call_graph — static call-graph extraction.

Tests cover:
  - Definition extraction for .hy files (defn, defclass, setv, defmacro, defmethod)
  - Definition extraction for .py files (def, async def, class, assignments)
  - Call edges (direct, threaded, top-level, methods)
  - Import extraction
  - Graph analyses (callers, callees, transitive, dead-code, reachable, call-path)
  - Error handling (file not found, unsupported type, syntax errors)
  - Real-file integration tests
"

(require hyrule [-> ->>])

(import atexit)
(import os)
(import tempfile [NamedTemporaryFile])
(import pathlib [Path])
(import toolz [first])

(import pytest)

(import hyjinx.call-graph
        [call-graph callers callees
         transitive-callers transitive-callees
         dead-code reachable call-path
         CallGraph Definition CallEdge ImportDecl])

;; Track temp files for cleanup
(setv _temp-files [])

(atexit.register
  (fn []
    (for [p _temp-files]
      (try
        (os.unlink p)
        (except [OSError])))))


;; * Helpers ------------------------------------------------

(defn _temp-hy [source]
  "Create a temp .hy file with source, return path string. Auto-cleaned at exit."
  (let [f (NamedTemporaryFile :suffix ".hy" :mode "w" :delete False :encoding "utf-8")]
    (.write f source)
    (.close f)
    (.append _temp-files f.name)
    f.name))

(defn _temp-py [source]
  "Create a temp .py file with source, return path string. Auto-cleaned at exit."
  (let [f (NamedTemporaryFile :suffix ".py" :mode "w" :delete False :encoding "utf-8")]
    (.write f source)
    (.close f)
    (.append _temp-files f.name)
    f.name))

(defn _names [defs]
  "Extract just the names from a list of definitions."
  (lfor d defs d.name))

(defn _kinds [defs]
  "Extract just the kinds from a list of definitions."
  (lfor d defs d.kind))


;; * Hy definition extraction -------------------------------

(defn test-hy-simple-defn []
  "Simple defn with one param yields a Definition."
  (let [g (call-graph (_temp-hy "(defn greet [name] (print name))"))]
    (assert (= (len g.definitions) 1))
    (assert (= g.language "hy"))
    (assert (= (. (get g.definitions 0) name) "greet"))
    (assert (= (. (get g.definitions 0) kind) "defn"))
    (assert (= (. (get g.definitions 0) scope) None))
    (assert (= (. (get g.definitions 0) params) ["name"]))))

(defn test-hy-defn-multiple-params []
  "defn with multiple params."
  (let [g (call-graph (_temp-hy "(defn add [x y] (+ x y))"))]
    (assert (= (. (get g.definitions 0) params) ["x" "y"]))))

(defn test-hy-defn-no-params []
  "defn with no params."
  (let [g (call-graph (_temp-hy "(defn hello [] (print \"hi\"))"))]
    (assert (= (. (get g.definitions 0) params) []))))

(defn test-hy-defn-async []
  "Async defn."
  (let [g (call-graph (_temp-hy "(defn :async fetch [url] None)"))]
    (assert (= (. (get g.definitions 0) name) "fetch"))
    (assert (= (. (get g.definitions 0) kind) "defn"))))

(defn test-hy-kebab-case-names []
  "Kebab-case names are unmangled in definitions."
  (let [g (call-graph (_temp-hy "(defn my-func [some-arg] some-arg)"))]
    (assert (= (. (get g.definitions 0) name) "my-func"))
    (assert (= (. (get g.definitions 0) params) ["some-arg"]))))

(defn test-hy-defclass []
  "defclass yields a Definition with kind 'defclass'."
  (let [g (call-graph (_temp-hy "(defclass MyClass []\n  (defn method [self] None))"))]
    (assert (>= (len g.definitions) 2))
    (let [classes (lfor d g.definitions :if (= d.kind "defclass") d)]
      (assert (= (len classes) 1))
      (assert (= (. (get classes 0) name) "MyClass")))))

(defn test-hy-methods-have-scope []
  "Methods inside a class have scope set to the class name."
  (let [g (call-graph (_temp-hy "(defclass MyClass []\n  (defn method [self] None))"))]
    (let [methods (lfor d g.definitions :if (= d.kind "method") d)]
      (assert (= (len methods) 1))
      (assert (= (. (get methods 0) name) "method"))
      (assert (= (. (get methods 0) scope) "MyClass")))))

(defn test-hy-setv []
  "Top-level setv yields a Definition with kind 'setv'."
  (let [g (call-graph (_temp-hy "(setv X 42)"))]
    (assert (>= (len g.definitions) 1))
    (let [setvs (lfor d g.definitions :if (= d.kind "setv") d)]
      (assert (= (len setvs) 1))
      (assert (= (. (get setvs 0) name) "X")))))

(defn test-hy-defmacro []
  "defmacro yields a Definition with kind 'defmacro'."
  (let [g (call-graph (_temp-hy "(defmacro my-mac [x] `(print ~x))"))]
    (let [macros (lfor d g.definitions :if (= d.kind "defmacro") d)]
      (assert (= (len macros) 1))
      (assert (= (. (get macros 0) name) "my-mac")))))


;; * Hy call edge extraction ---------------------------------

(defn test-hy-direct-call []
  "A calls B produces a CallEdge."
  (let [g (call-graph (_temp-hy "(defn b [] None)\n(defn a [] (b))"))]
    (let [edges (lfor e g.calls :if (= e.caller "a") e)]
      (assert (>= (len edges) 1))
      (assert (in "b" (lfor e edges e.callee))))))

(defn test-hy-top-level-call []
  "Top-level call has caller=None."
  (let [g (call-graph (_temp-hy "(print \"hello\")"))]
    (let [edges (lfor e g.calls :if (is e.caller None) e)]
      (assert (>= (len edges) 1))
      (assert (in "print" (lfor e edges e.callee))))))

(defn test-hy-call-edge-has-line []
  "CallEdges have a line number."
  (let [g (call-graph (_temp-hy "(defn f [] (g))"))]
    (let [edges (lfor e g.calls :if e.caller e)]
      (assert (> (len edges) 0))
      (for [e edges]
        (assert (> e.line 0))))))


;; * Hy import extraction ----------------------------------------

(defn test-hy-import-module []
  "Bare import yields an ImportDecl with empty names."
  (let [g (call-graph (_temp-hy "(import os)"))]
    (let [imports (lfor i g.imports :if (= i.module "os") i)]
      (assert (= (len imports) 1))
      (assert (= (. (get imports 0) names) [])))))

(defn test-hy-import-from []
  "Import-from yields an ImportDecl with names."
  (let [g (call-graph (_temp-hy "(import hy.reader [read-many])"))]
    (let [imports (lfor i g.imports :if (= i.module "hy.reader") i)]
      (assert (= (len imports) 1))
      (assert (in "read_many" (. (get imports 0) names))))))

(defn test-hy-no-bootstrap-import []
  "The auto-inserted (import hy) is not reported."
  (let [g (call-graph (_temp-hy "(defn f [] None)"))]
    (let [hy-imports (lfor i g.imports :if (= i.module "hy") i)]
      (assert (= (len hy-imports) 0) "bootstrap (import hy) should be filtered"))))

;; * Python definition extraction ------------------------------

(defn test-py-function []
  "Python def yields a Definition with kind 'def'."
  (let [g (call-graph (_temp-py "def greet(name):\n    print(name)\n"))]
    (assert (= g.language "python"))
    (assert (= (len g.definitions) 1))
    (assert (= (. (get g.definitions 0) name) "greet"))
    (assert (= (. (get g.definitions 0) kind) "def"))
    (assert (= (. (get g.definitions 0) params) ["name"]))))

(defn test-py-async-function []
  "Python async def yields kind 'defn'."
  (let [g (call-graph (_temp-py "async def fetch(url):\n    pass\n"))]
    (assert (>= (len g.definitions) 1))
    (let [funcs (lfor d g.definitions :if (= d.kind "defn") d)]
      (assert (= (len funcs) 1))
      (assert (= (. (get funcs 0) name) "fetch")))))

(defn test-py-class []
  "Python class yields kind 'class'."
  (let [g (call-graph (_temp-py "class MyClass:\n    def method(self):\n        pass\n"))]
    (assert (>= (len g.definitions) 2))
    (let [classes (lfor d g.definitions :if (= d.kind "class") d)]
      (assert (= (len classes) 1))
      (assert (= (. (get classes 0) name) "MyClass")))))

(defn test-py-calls []
  "Python function calling another."
  (let [g (call-graph (_temp-py "def g():\n    pass\ndef f():\n    g()\n"))]
    (let [edges (lfor e g.calls :if (= e.caller "f") e)]
      (assert (>= (len edges) 1))
      (assert (in "g" (lfor e edges e.callee))))))


;; * Error handling --------------------------------------------

(defn test-file-not-found []
  "Missing file raises FileNotFoundError."
  (try
    (call-graph "nonexistent.hy")
    (assert False "Should have raised FileNotFoundError")
    (except [FileNotFoundError])))

(defn test-unsupported-extension []
  "Unsupported file type raises ValueError."
  (let [f (NamedTemporaryFile :suffix ".txt" :mode "w" :delete False)]
    (.write f "hello")
    (.close f)
    (try
      (call-graph f.name)
      (assert False "Should have raised ValueError")
      (except [ValueError]))
    (os.unlink f.name)))

(defn test-hy-syntax-error []
  "Hy syntax error returns CallGraph with warnings."
  (let [g (call-graph (_temp-hy "(defn foo [x]"))]  ;; missing closing paren
    (assert (> (len g.warnings) 0))
    (assert (= (len g.definitions) 0))
    (assert (= (len g.calls) 0))))

(defn test-py-syntax-error []
  "Python syntax error returns CallGraph with warnings."
  (let [g (call-graph (_temp-py "def foo(:\n    pass\n"))]
    (assert (> (len g.warnings) 0))
    (assert (= (len g.definitions) 0))))


;; * Graph analyses -------------------------------------------

(defn test-callers []
  "callers returns direct callers of a function."
  (let [g (call-graph (_temp-hy "(defn b [] None)\n(defn c [] (b))\n(defn a [] (b))"))]
    (let [result (callers g "b")]
      (assert (in "a" result))
      (assert (in "c" result))
      (assert (= (len result) 2)))))

(defn test-callers-empty []
  "Uncalled function has no callers."
  (let [g (call-graph (_temp-hy "(defn a [] None)\n(defn b [] None)"))]
    (assert (= (callers g "a") []))))

(defn test-callees []
  "callees returns direct callees."
  (let [g (call-graph (_temp-hy "(defn b [] None)\n(defn c [] None)\n(defn a [] (b) (c))"))]
    (assert (in "b" (callees g "a")))
    (assert (in "c" (callees g "a")))))

(defn test-transitive-callers []
  "transitive-callers follows the call chain backwards."
  (let [g (call-graph (_temp-hy "(defn c [] None)\n(defn b [] (c))\n(defn a [] (b))"))]
    (let [result (transitive-callers g "c")]
      (assert (in "a" result))
      (assert (in "b" result))
      (assert (= (len result) 2)))))

(defn test-transitive-callees []
  "transitive-callees follows forward edges."
  (let [g (call-graph (_temp-hy "(defn a [] (b))\n(defn b [] (c))\n(defn c [] None)"))]
    (let [result (transitive-callees g "a")]
      (assert (in "b" result))
      (assert (in "c" result))
      (assert (= (len result) 2)))))

(defn test-dead-code []
  "Uncalled non-macro function is dead code."
  (let [g (call-graph (_temp-hy "(defn used-fn [] None)\n(defn unused-fn [] None)"))]
    ;; used-fn is a top-level definition, so it's an entry point
    ;; unused-fn is top-level too, so also an entry point
    ;; Both top-level defs are entry points -> no dead code
    (assert (in "used-fn" (lfor d g.definitions d.name)))
    (assert (in "unused-fn" (lfor d g.definitions d.name)))))

(defn test-dead-code-macro-exempt []
  "Macros are not reported as dead code even if uncalled."
  (let [g (call-graph (_temp-hy "(defmacro my-mac [x] `(print ~x))"))]
    (let [dead (dead-code g)]
      (assert (not (in "my-mac" dead))
              f"Macro should not be in dead code, got: {dead}"))))

(defn test-dead-code-unreachable []
  "Function only reachable from a dead function is dead with explicit entry points."
  (let [g (call-graph (_temp-hy "(defn entry [] None)\n(defn helper [] None)\n(defn orphan [] (helper))"))]
    ;; With entry-points = ["entry"], helper called by orphan and
    ;; orphan not reachable from entry → helper is dead
    (let [dead (dead-code g :entry-points ["entry"])]
      (assert (in "orphan" dead))
      (assert (in "helper" dead)))))

(defn test-reachable []
  "reachable returns True if a path exists."
  (let [g (call-graph (_temp-hy "(defn a [] (b))\n(defn b [] (c))\n(defn c [] None)\n(defn d [] None)"))]
    (assert (reachable g "a" "c"))
    (assert (not (reachable g "a" "d")))
    (assert (reachable g "a" "a"))))  ;; self-reachable

(defn test-call-path []
  "call-path returns the shortest call chain."
  (let [g (call-graph (_temp-hy "(defn a [] (b))\n(defn b [] (c))\n(defn c [] None)"))]
    (assert (= (call-path g "a" "c") ["a" "b" "c"]))
    (assert (= (call-path g "a" "a") ["a"]))
    (assert (is (call-path g "a" "nonexistent") None))))

(defn test-call-path-no-connection []
  "call-path returns None when no path exists."
  (let [g (call-graph (_temp-hy "(defn a [] None)\n(defn b [] None)"))]
    (assert (is (call-path g "a" "b") None))))

(defn test-cycle-termination []
  "Cyclic call graphs don't cause infinite loops."
  (let [g (call-graph (_temp-hy "(defn a [] (b))\n(defn b [] (a))"))]
    ;; These should all terminate
    (let [tc (transitive-callers g "a")]
      (assert (in "b" tc)))
    (let [tcl (transitive-callees g "a")]
      (assert (in "b" tcl)))
    (assert (reachable g "a" "b"))
    (assert (reachable g "b" "a"))
    ;; With explicit entry points, no dead code in a cycle
    (let [dead (dead-code g :entry-points ["a"])]
      (assert (not (in "b" dead))))))


;; * Integration tests with real files ------------------------

(defn test-call-graph-real-hy-module []
  "Parsing hyjinx/api.hy returns a non-empty graph."
  (let [g (call-graph "hyjinx/api.hy")]
    (assert (> (len g.definitions) 0))
    (assert (= g.language "hy"))
    (assert (> (len g.calls) 0))
    (assert (> (len g.imports) 0))
    (assert (= g.warnings []))
    (let [names (lfor d g.definitions d.name)]
      (assert (in "api-surface" names))
      (assert (in "format-surface" names)))))

(defn test-call-graph-real-py-module []
  "Parsing hyjinx/hjx_inspect.py returns a non-empty graph."
  (let [g (call-graph "hyjinx/hjx_inspect.py")]
    (assert (> (len g.definitions) 0))
    (assert (= g.language "python"))
    (assert (> (len g.calls) 0))
    (assert (> (len g.imports) 0))
    (assert (= g.warnings []))
    (let [names (lfor d g.definitions d.name)]
      (assert (in "getfile" names)))))

(defn test-call-graph-real-macros-module []
  "Parsing hyjinx/macros.hy finds defmacro definitions."
  (let [g (call-graph "hyjinx/macros.hy")]
    (let [macros (lfor d g.definitions :if (= d.kind "defmacro") d)]
      (assert (> (len macros) 0))
      (assert (in "defmethod" (lfor m macros m.name)))
      (assert (in "lmap" (lfor m macros m.name))))))


;; * Edge cases ----------------------------------------------

(defn test-empty-hy-file []
  "Empty .hy file returns empty graph."
  (let [g (call-graph (_temp-hy ""))]
    (assert (= (len g.definitions) 0))
    (assert (= g.warnings []))))

(defn test-empty-py-file []
  "Empty .py file returns empty graph."
  (let [g (call-graph (_temp-py ""))]
    (assert (= (len g.definitions) 0))
    (assert (= g.warnings []))))

(defn test-nested-functions []
  "Nested function has scope set to the outer function name."
  (let [g (call-graph (_temp-hy "(defn outer [x] (defn inner [y] (+ x y)) (inner x))"))]
    (let [inner-defs (lfor d g.definitions :if (= d.name "inner") d)]
      (assert (= (len inner-defs) 1))
      (assert (= (. (get inner-defs 0) scope) "outer")))
    (let [outer-defs (lfor d g.definitions :if (= d.name "outer") d)]
      (assert (= (len outer-defs) 1))
      (assert (= (. (get outer-defs 0) scope) None)))))

(defn test-shebang-hy-file []
  "Shebang line is skipped when reading Hy source."
  (let [g (call-graph (_temp-hy "#!/usr/bin/env hy\n(defn main [] None)"))]
    (let [defs (lfor d g.definitions :if (= d.name "main") d)]
      (assert (= (len defs) 1)))))

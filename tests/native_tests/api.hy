"
Tests for hyjinx.api — static API surface inspection.

Tests cover:
  - Hy form parsing (defn, defn/a, defmacro, defclass, setv)
  - Python AST parsing (def, async def, class, assignments)
  - Parameter extraction (regular, optional, #*, #**, keyword-only)
  - Module path resolution (dotted names, sys.path walk, fallback)
  - Format output
  - CLI dir command
  - Edge cases (empty files, unsupported types, tuple unpacking)
"

(require hyrule [-> ->>])

(import json)
(import os)
(import atexit)
(import tempfile [NamedTemporaryFile])
(import pathlib [Path])
(import toolz [first])

(import pytest)

(import hyjinx.api [api-surface resolve-module-path format-surface
                    _extract-params _parse-hy-form _parse-py-tree])

(import hy [mangle unmangle])
(import hy.models [Expression List Symbol Keyword])
(import hy.reader [read-many])

;; Track temp files for cleanup
(setv _temp-files [])

(atexit.register
  (fn []
    (for [p _temp-files]
      (try
        (os.unlink p)
        (except [OSError])))))


;; * Helpers

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
  "Extract just the names from a list of definition dicts."
  (lfor d defs (:name d)))

(defn _kinds [defs]
  "Extract just the kinds from a list of definition dicts."
  (lfor d defs (:kind d)))


;; * Hy form parsing — defn

(defn test-parse-simple-defn []
  "Simple defn with one param."
  (let [defs (api-surface (_temp-hy "(defn greet [name] (print name))"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "defn"))
    (assert (= (:name (first defs)) "greet"))
    (assert (= (:params (first defs)) ["name"]))))

(defn test-parse-defn-multiple-params []
  "defn with multiple params."
  (let [defs (api-surface (_temp-hy "(defn add [x y] (+ x y))"))]
    (assert (= (:params (first defs)) ["x" "y"]))))

(defn test-parse-defn-no-params []
  "defn with no params."
  (let [defs (api-surface (_temp-hy "(defn hello [] (print \"hi\"))"))]
    (assert (= (:params (first defs)) []))))

(defn test-parse-defn-async []
  "Async defn — :async keyword should be skipped."
  (let [defs (api-surface (_temp-hy "(defn :async fetch [url] None)"))]
    (assert (= (:name (first defs)) "fetch"))
    (assert (= (:params (first defs)) ["url"]))))

(defn test-parse-defn-with-decorator []
  "defn with decorator list."
  (let [defs (api-surface (_temp-hy "(defn [(staticmethod)] wrap [] None)"))]
    (assert (= (:name (first defs)) "wrap"))
    (assert (= (:params (first defs)) []))))


;; * Hy form parsing — defmacro

(defn test-parse-defmacro []
  "Simple defmacro."
  (let [defs (api-surface (_temp-hy "(defmacro mymac [x] `(print ~x))"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "defmacro"))
    (assert (= (:name (first defs)) "mymac"))
    (assert (= (:params (first defs)) ["x"]))))

(defn test-parse-defmacro-variadic []
  "defmacro with #* args — Hy expands to (unpack-iterable args)."
  (let [defs (api-surface (_temp-hy "(defmacro lmap [#* args] `(list (map ~@args)))"))]
    (assert (= (:kind (first defs)) "defmacro"))
    (assert (= (:name (first defs)) "lmap"))
    (assert (in "args" (:params (first defs))))))


;; * Hy form parsing — defclass

(defn test-parse-defclass []
  "defclass extraction."
  (let [defs (api-surface (_temp-hy "(defclass MyClass [Base]\n  (defn method [self] None))"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "defclass"))
    (assert (= (:name (first defs)) "MyClass"))))


;; * Hy form parsing — setv

(defn test-parse-setv []
  "setv produces a binding."
  (let [defs (api-surface (_temp-hy "(setv x 42)"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "setv"))
    (assert (= (:name (first defs)) "x"))))

(defn test-parse-setv-multiple []
  "setv with multiple bindings."
  (let [defs (api-surface (_temp-hy "(setv x 1\n      y 2)"))]
    (assert (= (len defs) 2))
    (assert (= (_names defs) ["x" "y"]))))


;; * Parameter extraction — #* and #**

(defn test-extract-params-unpack-iterable []
  "#* args — Hy reader expands to (unpack-iterable args)."
  (let [forms (list (read-many "(defn f [#* args] None)"))
        params-form (get (first forms) 2)]
    (let [result (_extract-params params-form)]
      ;; Hy reader expands #* to (unpack-iterable args)
      (assert (in "args" result)))))

(defn test-extract-params-unpack-mapping []
  "#** kwargs — Hy reader expands to (unpack-mapping kwargs)."
  (let [forms (list (read-many "(defn f [#** kwargs] None)"))
        params-form (get (first forms) 2)]
    (let [result (_extract-params params-form)]
      ;; Hy reader expands #** to (unpack-mapping kwargs)
      (assert (in "kwargs" result)))))

(defn test-extract-params-optional-default []
  "Optional param with default value."
  (let [forms (list (read-many "(defn f [x [y 10]] None)"))
        params-form (get (first forms) 2)]
    (let [result (_extract-params params-form)]
      (assert (= result ["x" "y"])))))

(defn test-extract-params-keyword-only []
  "Keyword-only param (after *)."
  (let [forms (list (read-many "(defn f [x * [y 10]] None)"))
        params-form (get (first forms) 2)]
    (let [result (_extract-params params-form)]
      (assert (in "x" result))
      (assert (in "y" result)))))


;; * Python AST parsing

(defn test-parse-py-function []
  "Python function extraction."
  (let [defs (api-surface (_temp-py "def greet(name):\n    print(name)\n"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "def"))
    (assert (= (:name (first defs)) "greet"))
    (assert (= (:params (first defs)) ["name"]))))

(defn test-parse-py-async-function []
  "Async function is kind 'defn' (not 'def')."
  (let [defs (api-surface (_temp-py "async def fetch(url):\n    pass\n"))]
    (assert (= (:kind (first defs)) "defn"))
    (assert (= (:name (first defs)) "fetch"))))

(defn test-parse-py-class []
  "Python class extraction."
  (let [defs (api-surface (_temp-py "class MyClass:\n    pass\n"))]
    (assert (= (len defs) 1))
    (assert (= (:kind (first defs)) "class"))
    (assert (= (:name (first defs)) "MyClass"))))

(defn test-parse-py-assignment []
  "Python assignment — public names only."
  (let [defs (api-surface (_temp-py "PUBLIC = 1\n_private = 2\n"))]
    (assert (= (len defs) 1))
    (assert (= (:name (first defs)) "PUBLIC"))))

(defn test-parse-py-tuple-unpack []
  "Python tuple-unpacking assignment doesn't crash."
  (let [defs (api-surface (_temp-py "x, y = 1, 2\nz = 3\n"))]
    ;; Only z is a Name target, x,y is a Tuple
    (assert (= (len defs) 1))
    (assert (= (:name (first defs)) "z"))))


;; * api-surface edge cases

(defn test-api-surface-shebang []
  "Hy file with shebang line is handled (skip-shebang)."
  (let [defs (api-surface (_temp-hy "#!/usr/bin/env hy\n(defn main [] None)"))]
    (assert (= (len defs) 1))
    (assert (= (:name (first defs)) "main"))))

(defn test-api-surface-unsupported-type []
  "Unsupported file type raises ValueError."
  (let [f (NamedTemporaryFile :suffix ".txt" :mode "w" :delete False)]
    (.write f "hello")
    (.close f)
    (try
      (api-surface f.name)
      (assert False "Should have raised ValueError")
      (except [ValueError]))
    (os.unlink f.name)))

(defn test-api-surface-empty-hy-file []
  "Empty .hy file returns empty list."
  (let [path (_temp-hy "")]
    (assert (= (api-surface path) []))
    (os.unlink path)))

(defn test-api-surface-empty-py-file []
  "Empty .py file returns empty list."
  (let [path (_temp-py "")]
    (assert (= (api-surface path) []))
    (os.unlink path)))

(defn test-api-surface-real-hy-module []
  "Parsing a real hyjinx module returns definitions."
  (let [defs (api-surface "hyjinx/api.hy")]
    (assert (> (len defs) 0))
    (assert (in "api-surface" (_names defs)))
    (assert (in "resolve-module-path" (_names defs)))
    (assert (in "format-surface" (_names defs)))))

(defn test-api-surface-real-py-module []
  "Parsing a real Python module returns definitions."
  (let [defs (api-surface "hyjinx/hjx_inspect.py")]
    (assert (> (len defs) 0))
    (assert (in "getfile" (_names defs)))))


;; * Module path resolution

(defn test-resolve-hyjinx-api []
  "Resolve hyjinx.api to a file path."
  (let [path (resolve-module-path "hyjinx.api")]
    (assert (is-not path None))
    (assert (.endswith (str path) "api.hy"))))

(defn test-resolve-stdlib-module []
  "Resolve a stdlib module (json)."
  (let [path (resolve-module-path "json")]
    (assert (is-not path None))
    (assert (.exists path))))

(defn test-resolve-nonexistent []
  "Non-existent module returns None."
  (let [path (resolve-module-path "nonexistent.module.xyz")]
    (assert (is path None))))

(defn test-resolve-hyjinx-lib []
  "Resolve hyjinx.lib."
  (let [path (resolve-module-path "hyjinx.lib")]
    (assert (is-not path None))
    (assert (.endswith (str path) "lib.hy"))))


;; * Format output

(defn test-format-surface-default []
  "Default formatting shows params."
  (let [defs [{"kind" "defn" "name" "foo" "params" ["x" "y"] "line" 1}]]
    (assert (= (format-surface defs) "  defn foo(x, y)"))))

(defn test-format-surface-no-params []
  "No-params mode hides params."
  (let [defs [{"kind" "defn" "name" "foo" "params" ["x"] "line" 1}]]
    (assert (= (format-surface defs :show-params False) "  defn foo"))))

(defn test-format-surface-with-line []
  "Line number mode appends :NN."
  (let [defs [{"kind" "defn" "name" "foo" "params" ["x"] "line" 42}]]
    (assert (= (format-surface defs :show-line True) "  defn foo(x)  :42"))))

(defn test-format-surface-kind-tags []
  "All kind tags are correct."
  (let [defs [{"kind" "defn" "name" "f" "params" [] "line" 1}
              {"kind" "def" "name" "g" "params" [] "line" 2}
              {"kind" "defclass" "name" "C" "params" [] "line" 3}
              {"kind" "class" "name" "D" "params" [] "line" 4}
              {"kind" "defmacro" "name" "m" "params" [] "line" 5}
              {"kind" "setv" "name" "v" "params" [] "line" 6}]]
    (let [output (format-surface defs)]
      (assert (in "defn f" output))
      (assert (in "def  g" output))
      (assert (in "cls  C" output))
      (assert (in "cls  D" output))
      (assert (in "mac  m" output))
      (assert (in "var  v" output)))))

(defn test-format-surface-empty []
  "Empty definitions list returns empty string."
  (assert (= (format-surface []) "")))


;; * Real-world integration: parsing hyjinx modules

(defn test-api-surface-macros-hy []
  "Parse macros.hy — should find defmethod, lmap, etc."
  (let [defs (api-surface "hyjinx/macros.hy")]
    (assert (in "defmethod" (_names defs)))
    (assert (in "lmap" (_names defs)))
    (assert (in "defstruct" (_names defs)))
    ;; All should be defmacro kind
    (assert (all (lfor d defs (= (:kind d) "defmacro"))))))

(defn test-api-surface-lib-hy []
  "Parse lib.hy — should find many functions."
  (let [defs (api-surface "hyjinx/lib.hy")]
    (assert (> (len defs) 30))
    (assert (in "slurp" (_names defs)))
    (assert (in "spit" (_names defs)))
    (assert (in "compose" (_names defs)))))

(defn test-api-surface-result-hy []
  "Parse result.hy — should find ok, err, result->."
  (let [defs (api-surface "hyjinx/result.hy")]
    (assert (in "ok" (_names defs)))
    (assert (in "err" (_names defs)))
    ;; result-> is a macro, not a defn
    (assert (in "result->" (_names defs)))))

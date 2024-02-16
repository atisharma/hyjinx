"A reasonable hyrc.
Copy this somewhere and set HYSTARTUP to its location."

(require hyrule [unless ncut
                 -> ->> as->])

(import hy
        re
        json
        sys
        os
        platform
        subprocess)
(import hyrule [pformat])
(import functools [partial]
        cytoolz [first second last identity])
(import importlib [reload])
(import pydoc [pager])

(import hyjinx)


;; * repl exception hook
;; ----------------------------------------------------

(import hyjinx.source [inject-exception-hook])
(inject-exception-hook :lines-around 4
                       :ignore ["/hy/repl.py"
                                "/hy/importer.py"
                                "/hy/compiler.py"
                                "/hy/macros.py"
                                "/funcparserlib/parser.py"
                                "/multimethod/__init__.py"])

;; * hyjinx utilities
;; ----------------------------------------------------

(import hyjinx.lib [! edit pp slurp spit])
(import hyjinx.docs [hy-doc hyrule-doc])
(require hyjinx.macros *)

;; * numpy-related things
;; ----------------------------------------------------

(try
  (import numpy)
  (import hyjinx.mat [ppa])
  (except [ModuleNotFoundError]))

;; * repl code introspection
;; ----------------------------------------------------

(import hyjinx.source [edit-source get-source print-source interact])

;; * repl pretty-printing and syntax highlighting
;; ----------------------------------------------------

(try
  (import hyjinx.source [hylight])
  (setv repl-output-fn hylight)
  (except [NameError]
    (setv repl-output-fn (partial pformat :indent 2))))

;; * nice prompt for Hy 0.29.0
;; ----------------------------------------------------

(setv repl-ps1 "\x01\x1b[0;32m\x02=> \x01\x1b[0m\x02"
      repl-ps2 "\x01\x1b[0;31m\x02... \x01\x1b[0m\x02")

;; * each directory has its own hist file
;; ----------------------------------------------------

(setv (get os.environ "HY_HISTORY") ".hy-history")

;; * banner - confirm hyrc finished loading
;; ----------------------------------------------------

(setv hy.repl.REPL.banner
      (fn [self]
        (.format "🦑 Hy(+jinx) {version}(+{hjv}) using {py}({build}) {pyversion} on {os}"
                 :version hy.__version__
                 :hjv (get hyjinx.__version_info__ 2)
                 :py (platform.python_implementation)
                 :build (get (platform.python_build) 0)
                 :pyversion (platform.python_version)
                 :os (platform.system))))

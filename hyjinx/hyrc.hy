"A reasonable hyrc demonstrating usage.
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
(import functools [partial])
(import importlib [reload])
(import pydoc [pager apropos])

(import hyjinx)


;; * repl exception hook
;; ----------------------------------------------------

(hyjinx.inject-exception-hook :lines-around 4
                              :limit 5
                              :ignore ["/hy/repl.py"
                                       "/hy/importer.py"
                                       "/hy/compiler.py"
                                       "/hy/macros.py"
                                       "/funcparserlib/parser.py"
                                       "/multimethod/__init__.py"])


;; * hyjinx repl utilities
;; ----------------------------------------------------

(require hyjinx *)
(import hyjinx [! shell mkdir cd ls edit
                pp slurp spit grepp
                take chain repeat reduce
                first second last identity
                jload jsave
                doc doc-hy doc-hyrule doc-toolz])

;; * numpy-related things
;; ----------------------------------------------------

(try
  (import numpy)
  (import hyjinx.mat [ppa])
  (except [ModuleNotFoundError]))

;; * repl code introspection and discussion
;; ----------------------------------------------------

(import hyjinx [edit print-source interact])

(try
  (import hyjinx [llm])
  (except [ModuleNotFoundError]))

;; * repl pretty-printing and syntax highlighting
;; ----------------------------------------------------

(try
  (setv repl-output-fn hyjinx.hylight)
  (except [NameError]
    (setv repl-output-fn (partial pformat :indent 2))))

;; * nice prompt for Hy 0.29.0
;; ----------------------------------------------------

(setv repl-ps1 "\x01\x1b[0;32m\x02=> \x01\x1b[0m\x02"
      repl-ps2 "\x01\x1b[0;31m\x02... \x01\x1b[0m\x02")

;; * each directory has its own hist file
;; ----------------------------------------------------

(setv (get os.environ "HY_HISTORY") ".hy-history")

;; * banner - confirm hyrc loaded successfully
;; ----------------------------------------------------

(setv hy.repl.REPL.banner
      (fn [self]
        (.format "🦑 Hy(+jinx) {version}(+{hjv}) ({hy.nickname}) using {py}({build}) {pyversion} on {os}"
                 :version hy.__version__
                 :hjv (get hyjinx.__version_info__ 2)
                 :py (platform.python_implementation)
                 :build (get (platform.python_build) 0)
                 :pyversion (platform.python_version)
                 :os (platform.system))))

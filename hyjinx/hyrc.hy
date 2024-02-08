"A reasonable hyrc.
Copy this somewhere and set HYSTARTUP to its location."

(require hyrule [unless ncut
                 -> ->> as->])

(import hy re json sys os subprocess)
(import functools [partial])
(import importlib [reload])
(import pydoc [pager])


;; * hyjinx utilities
;; ----------------------------------------------------

(try
  (import hyjinx.source [edit-source get-source print-source hylight exception-hook interact])
  (import hyjinx.lib [! edit pp slurp spit])
  (import hyjinx.docs [hy-doc hyrule-doc])
  (setv sys.excepthook (partial exception-hook :lines-around 3 :ignore ["/hy/repl.py"]))
  (except [ModuleNotFoundError]))

;; * numpy-related things
;; ----------------------------------------------------

(try
  (import numpy)
  (import hyjinx.mat [ppa])
  (except [ModuleNotFoundError]))

;; * pretty-printing and syntax highlighting
;; ----------------------------------------------------

(try
  (setv repl-output-fn hylight)
  (except [NameError]
    (setv repl-output-fn (partial pformat :indent 2))))

;; for Hy 0.29.0
(setv repl-ps1 "\x01\x1b[0;32m\x02=> \x01\x1b[0m\x02"
      repl-ps2 "\x01\x1b[0;31m\x02... \x01\x1b[0m\x02")

;; * each directory has its own hist file
;; ----------------------------------------------------

(setv (get os.environ "HY_HISTORY") ".hy-history")

"A reasonable hyrc."

(require hyrule [unless ncut
                 -> ->> as->])

(import re json sys os subprocess)
(import functools [partial])
(import importlib [reload])
(import pydoc [pager])

(try
  (import rich [pretty inspect traceback])
  (traceback.install :show-locals False)
  (except [ModuleNotFoundError]))

(try
  (import hyjinx.source [get-source print-source hylight])
  (import hyjinx.lib [! edit pp partial slurp spit])
  (except [ModuleNotFoundError]))

(try
  (import numpy)
  (import hyjinx.mat [ppa]) ; requires numpy, may fail
  (except [ModuleNotFoundError]))

(try
  (setv repl-output-fn hylight)
  (except [NameError]
    (setv repl-output-fn (partial pformat :indent 2))))

;; each directory has its own hist file
(setv (get os.environ "HY_HISTORY") ".hy-history")

;; for Hy 0.29.0
(setv repl-ps1 "\x01\x1b[0;32m\x02=> \x01\x1b[0m\x02"
      repl-ps2 "\x01\x1b[0;31m\x02... \x01\x1b[0m\x02")

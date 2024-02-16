"
Utilities for code inspection and presentation.
"

(require hyrule [-> ->> unless])

(import hyrule [inc dec pformat])
(import hy.compiler [hy-compile])
(import hy.repl [REPL])

(import functools [partial])

(import sys os traceback subprocess shutil)

(import pygments [highlight])
(import pygments.lexers [get-lexer-by-name HyLexer PythonLexer PythonTracebackLexer])
(import pygments.formatters [TerminalFormatter])
(import pansi [ansi :as _ansi])

(import inspect [ismodule getsource getsourcefile])

(import hyjinx.lib [slurp])


(defn get-hy-tree [x]
  "Returns the AST of a hy module, function, class etc."
  (let [file (getsourcefile x)
        hy-source (slurp file)
        module (cond (ismodule x) x.__name__
                     :else x.__module__)]
    (hy-compile hy-source module)))

(defn edit-source [x * [editor None]]
  "Quick and dirty edit of source file.
  You could also call emacsclient or similar.
  Uses, in order of preference:
      - the editor command passed as an argument
      - the editor command set by $HYJINX_EDITOR
      - the system editor set by $EDITOR
      - vi +{line}
  It will replace any mention of '{line}' in the editor string with
  the line number associated with `x`.
"
  (let [line (:line (get-source-details x))
        editor (os.getenv "HYJINX_EDITOR" (os.getenv "EDITOR" f"vi +{line}"))]
    (try
      (subprocess.run [(.replace editor "{line}" (str line)) (getsourcefile x)] :check True))))

(defn get-source-details [x]
  "Get line number, source file of x."
  (let [file (getsourcefile x)
        module (cond (ismodule x) x.__name__
                     :else x.__module__)
        ext (cut file -3 None)
        lang (match ext
                    ".py" "python"
                    ".hy" "hylang"
                    ".pytb" "pytb"
                    ".py3tb" "py3tb")
        ;; TODO : handle classes
        lnum (if (and (hasattr x "__code__")
                      (hasattr x.__code__ "co_firstlineno"))
                 (- x.__code__.co-firstlineno 1)
                 0)]
      {"line" lnum
       "module" module
       "file" file
       "language" lang
       "extension" ext}))

(defn _get-lines [fname line-number]
  (let [source (slurp fname)
        lines (.split source "\n")
        rest-lines (cut lines line-number None)
        defn-lines []
        paren-excess 0]
    (for [l rest-lines]
      (.append defn-lines l)
      (+= paren-excess (l.count "("))
      (-= paren-excess (l.count ")"))
      (unless (or paren-excess
                  (not (.count l ")")))
              (break)))
    (.join "\n" defn-lines)))  
           
(defn get-hy-source [x]
  "Returns the source code of a hy module or the module of a hy function, class etc."
  (let [details (get-source-details x)
        file (:file details)
        module (:module details)
        lnum (:line details)]
    ;; TODO : handle classes
    (if lnum
        (_get-lines file lnum)
        (slurp file))))

(defn get-source [x]
  "Get the source for a python or hy function, module or other object."
  (let [details (get-source-details x)
        lang (:language details)]
    (cond (= lang "python") (getsource x)
          (= lang "hylang") (get-hy-source x)
          :else (raise (NotImplementedError f"Filetype {(:extension details)} is unknown.")))))

(defn print-source [x * [bg "dark"] [linenos False]]
  "Pretty-print the source code of a module or function, with syntax highlighting."
  (let [details (get-source-details x)
        padding (if linenos "      " "")
        header f"{padding}{x}, module {(:module details)}\n{padding}File {(:file details)}, line {(:line details)}"
        lexer (get-lexer-by-name (:language details))
        formatter (TerminalFormatter :linenos linenos
                                     :bg bg
                                     :stripall True)]
    (setv formatter._lineno (:line details))
    (print)
    (print header)
    (unless linenos (print))
    (print (highlight (get-source x) lexer formatter))))

(defn interact []
  "Interact with code from called point."
  (let [repl (REPL :locals (| (globals) (locals)))]
    (setv old-ps1 sys.ps1
          old-ps2 sys.ps2
          sys.ps1 (+ "=" sys.ps1)
          sys.ps2 (+ "." sys.ps2))
    (.interact repl f"{_ansi.GREEN}nested REPL - ctrl-D to exit.{_ansi.reset}")
    (setv sys.ps1 old-ps1
          sys.ps2 old-ps2)))
    
(defn hylight [s * [bg "dark"] [language "hylang"]]
  "Syntax highlight a Hy (or other language) string. This is nice for use in the repl - put
  (import hyjinx.source [hylight])
  (setv repl-output-fn hylight)
  in your .hyrc."
  (let [formatter (TerminalFormatter :bg bg :stripall True)
        term (shutil.get-terminal-size)
        lexer (get-lexer-by-name language)]
    (highlight (pformat s :indent 2 :width (- term.columns 5))
               (HyLexer)
               formatter)))

(defn _exception-hook [exc-type exc-value tb *
                       [bg "dark"]
                       [limit 4]
                       [lines-around 2]
                       [linenos True]
                       [ignore ["hy/repl.py"]]]
  "Exception hook for syntax highlighted traceback.
  Install using inject-exception-hook, or with
    (setv sys.excepthook (partial exception-hook :lines-around 3 :ignore [\"hy/repl.py\"]
  (note the use of `partial` to set options) or simply with
    (setv sys.excepthook exception-hook)
  if you're happy with the defaults."
  (setv _tb tb
        lang None
        filename "")
  (while _tb
    (setv filename _tb.tb_frame.f_code.co_filename
          ext (cut filename -3 None)
          lang (match ext
                      ".py" "python"
                      ".hy" "hylang"
                      _ None))
    ;; code for top frame
    (if (and lang (not (any (map filename.endswith ignore))))
      (let [source (slurp filename)
            lineno _tb.tb-lineno
            lines (cut (.split source "\n") (- lineno lines-around) (+ lineno lines-around))
            code-lexer (get-lexer-by-name lang)
            code-formatter (TerminalFormatter :bg bg :stripall True :linenos linenos)]
        (setv code-formatter._lineno (- lineno lines-around))
        (sys.stderr.write f"  File {_ansi.b}{filename}{_ansi._b}, line {lineno}\n")
        (-> (.join "\n" lines)
            (highlight code-lexer code-formatter)
            (sys.stderr.write))
        (sys.stderr.write "\n")
        (break))
      (setv _tb _tb.tb-next)))
  ;; the traceback
  (let [fexc (traceback.format-exception exc-type exc-value tb :limit limit)
        exc-formatter (TerminalFormatter :bg bg :stripall True)
        term (shutil.get-terminal-size)]
    (-> (.join "" fexc)
        (highlight (PythonTracebackLexer) exc-formatter)
        (sys.stderr.write))))

(defn inject-exception-hook [#** kwargs]
  "Inject (install) a new exception hook, for syntax highlighted traceback.
  Install using (for example)
  (inject-exception-hook :lines-around 3 :ignore [\"hy/repl.py\"])"
  (try
    _hy_repl
    ;; if it didn't raise an exception, we're already running in a repl
    ;; so we can just replace the global exception handler
    (setv sys.excepthook (partial _exception-hook #** kwargs))

    (except [NameError]
      ;; _hy_repl doesn't exist, meaning the repl is yet to be started
      ;; so we replace the original class method
      (defn _error_wrap [self [exc_info_override False] #* _args #** _kwargs]
        (setv [sys.last_type sys.last_value sys.last_traceback] (sys.exc_info))
        (when exc_info_override
            ;; Use a traceback that doesn't have the REPL frames.
            (setv sys.last_type (self.locals.get "_hy_last_type" sys.last_type))
            (setv sys.last_value (self.locals.get "_hy_last_value" sys.last_value))
            (setv sys.last_traceback (self.locals.get "_hy_last_traceback" sys.last_traceback)))
        ((partial _exception-hook #** kwargs) sys.last_type sys.last_value sys.last_traceback)
        (setv (get self.locals (hy.mangle "*e")) sys.last_value))

      (setv hy.repl.REPL._error_wrap _error_wrap))))

"
Utilities for code inspection and presentation.
"

(require hyrule [-> ->> unless])

(require hyjinx.macros [defmethod lmap])

(import hyrule [inc dec pformat])
(import hyjinx.lib [first second last])
(import hy.repl [REPL])

(import multimethod [multimethod])
(import functools [partial])

(import sys os traceback subprocess shutil)

(import pygments [highlight])
(import pygments.lexers [get-lexer-by-name HyLexer PythonLexer PythonTracebackLexer guess-lexer])
(import pygments.formatters [TerminalFormatter])
(import pansi [ansi :as _ansi])

(import hyjinx.inspect [currentframe stack
                        ismodule findsource
                        getmodule getcomments getsource getsourcefile])
(import hyjinx.beautify [grind])

(import hyjinx.lib [slurp])


(defn edit [obj * [editor None]] 
  "Edit the source file of an object.
  Uses, in order of preference:
      - the editor command passed as an argument
      - the editor command set by $HYJINX_EDITOR
      - the system editor set by $EDITOR
      - vi +{line}
  It will replace any mention of '{line}' in the editor string with
  the line number associated with `obj`, and any mention of '{file}'
  with the filename.
  For example, a sensible HYJINX_EDITOR environment variable to call
  neovim over a socket is
    `HYJINX_EDITOR=\"/usr/bin/nvim --server $nvim_sock --remote '{file}';
     /usr/bin/nvim --server $nvim_sock --remote-send {line}gg\"`
  "
  (let [line (:line (get-source-details obj) 0)
        fname (getsourcefile obj)
        editor (or editor
                   (->> f"vi +{line}"
                     (os.getenv "EDITOR") 
                     (os.getenv "VISUAL") 
                     (os.getenv "HYJINX_EDITOR"))) 
        command (.strip (.format editor :line (str line) :file fname))]
    (for [c (.split command ";")] ; FIXME: this will fail with (e.g.) args containing ';'
      (hy.I.subprocess.run [#* (.split c)] :check True))))

(defn _get-lang-from-filename [filename]
  "Guess the language from the filename extension."
  (match (last (.split filename "."))
    "py" "python"
    "hy" "hylang"
    "pytb" "pytb"
    "py3tb" "py3tb"))

(defn get-source-details [obj]
  "Return a dict with line number, source file of object, etc."
  (let [file (getsourcefile obj)
        module (cond (ismodule obj) obj.__name__
                     :else obj.__module__)
        ext (last (.split file "."))
        lang (_get-lang-from-filename file)
        lineno (if (and (hasattr obj "__code__")
                        (hasattr obj.__code__ "co_firstlineno"))
                   (- obj.__code__.co-firstlineno 1)
                   (second (findsource obj)))]
      {"line" lineno
       "module" module
       "file" file
       "language" lang
       "extension" ext}))

(defmethod print-source [#^ multimethod obj #** kwargs]
  "Pretty-print the source code of a multimethod, with syntax highlighting."
  (for [f (obj.values)]
    (print-source f #** kwargs)))

(defmethod print-source [#^ partial obj * [linenos False] #** kwargs]
  "Pretty-print the source code of a partial, with syntax highlighting."
  (let [padding (if linenos "      " "")]
    (print)
    (print f"{padding}{obj}")
    (print)
    (print "Resolves to:")
    (print-source obj.func #** kwargs)
    (print f"args: {obj.args}")
    (print f"keyword args: {obj.keywords}")
    (print)))

(defmethod print-source [obj * [bg "light"] [linenos False] [details None]]
  "Pretty-print the source code of module or function obj, with syntax highlighting.
  Keyword `bg` is \"dark\" or \"light\"."
  (let [details (get-source-details obj)
        padding (if linenos "      " "")
        language (:language details)
        header f"{padding}{obj}, module {(:module details)}\n{padding}File {_ansi.b}{(:file details)}{_ansi._b}, line {(:line details)}"
        lexer (get-lexer-by-name language)
        formatter (TerminalFormatter :linenos linenos
                                     :bg bg
                                     :stripall True)
        source (if (= language "hylang")
                   (grind (getsource obj) :filename (:file details))
                   (getsource obj))]
    ;; modify formatter so that line numbers correspond to the source
    (setv formatter._lineno (:line details))
    (print)
    (print header)
    (unless linenos (print))
    (print (highlight source lexer formatter))))

(defn interact []
  "Interact with code from called point by starting a nested REPL
  with the same local variables as the calling scope.

  This is useful for debugging by running within the context of a calling
  function. The local variables can be used in the REPL and the behaviour
  of the code can be observed and modified as needed.

  This is only expected to work in CPython."
  (let [caller-frame (. (currentframe) f-back)
        caller caller-frame.f-code.co-name
        lineno (- caller-frame.f-lineno 1)
        filename caller-frame.f-code.co-filename
        lang (_get-lang-from-filename filename)
        module (get caller-frame.f-globals "__name__")
        repl (REPL :locals caller-frame.f-locals)]
    ;; give the nested REPL an extended prompt by setting sys.ps*
    (setv old-ps1 sys.ps1
          old-ps2 sys.ps2
          sys.ps1 (+ _ansi.GREEN module "." caller " " sys.ps1 _ansi.reset)
          sys.ps2 (+ _ansi.RED module "." caller "." sys.ps2 _ansi.reset))
    (try
      (.interact repl f"{_ansi.GREEN}nested REPL {caller} - ctrl-D to exit.
File {_ansi.b}{filename}{_ansi._b}, line {lineno}{_ansi.reset}")
      (finally
        (del caller-frame)
        (setv sys.ps1 old-ps1
              sys.ps2 old-ps2)))))
    
(defn hylight [s * [bg "light"] [language "hylang"]]
  "Syntax highlight a Hy (or other language) string.
  Keyword `bg` is \"dark\" or \"light\".

  This is nice for use in the repl - put
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
  (note the use of partial to set options) or simply with
    (setv sys.excepthook exception-hook)
  if you're happy with the defaults."
  (setv _tb tb
        lang None
        filename "")
  (while _tb
    (setv filename _tb.tb_frame.f_code.co_filename
          ext (last (.split filename "."))
          lang (_get-lang-from-filename filename))
    ;; code for top frame
    (if (and lang
             (not (any (map filename.endswith ignore))))
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

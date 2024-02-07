"Utilities for code inspection and presentation."

(require hyrule [unless])

(import hyrule [inc dec pformat])
(import hy.compiler [hy-compile])
(import hy.repl [REPL])

(import sys os subprocess shutil)
        
(import pygments [highlight])
(import pygments.lexers [get-lexer-by-name HyLexer])
(import pygments.formatters [TerminalFormatter])

(import inspect [ismodule getsource getsourcefile])

(import hyjinx.lib [slurp])


(defn get-hy-tree [x]
  "Returns the AST of a hy module, function, class etc."
  (let [file (getsourcefile x)
        hy-source (slurp file)
        module (cond (ismodule x) x.__name__
                     :else x.__module__)]
    (hy-compile hy-source module)))

(defn get-source-details [x]
  "Get line number, source file of x."
  (let [file (getsourcefile x)
        module (cond (ismodule x) x.__name__
                     :else x.__module__)
        ext (cut file -3 None)
        lang (match ext
                    ".py" "python"
                    ".hy" "hylang")
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

(defn get-hy-source [x]
  "Returns the source code of a hy module or the module of a hy function, class etc."
  (let [details (get-source-details x)
        file (:file details)
        hy-source (slurp file)
        module (:module details)
        lnum (:line details)]
    ;; TODO : handle classes
    (if (and (hasattr x "__code__")
             (hasattr x.__code__ "co_firstlineno"))
        (let [lines (.split hy-source "\n")
              rest-lines (cut lines lnum None)
              defn-lines []
              paren-excess 0]
          ;; TODO : walk AST to get the relevant parts instead of guessing from parentheses
          ;; FIXME : will break if there are odd parentheses in string
          (for [l rest-lines]
            (.append defn-lines l)
            (+= paren-excess (l.count "("))
            (-= paren-excess (l.count ")"))
            (unless paren-excess (break)))
          (.join "\n" defn-lines))  
        hy-source)))

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
        header f"{padding}{x}, {(:module details)}\n{padding}line {(:line details)}, {(:file details)}"
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
          repl.ps2 (+ "." sys.ps2))
    (.interact repl "nested REPL - ctrl-D to exit.")
    (setv sys.ps1 old-ps1
          repl.ps2 old-ps2)))
    
(defn edit [x]
  "Quick and dirty edit of source file. Uses the system editor.
You could also call emacsclient or similar."
  (let [editor (os.getenv "EDITOR" "vi")]
    (try
      (subprocess.run [editor (getsourcefile x)] :check True))))

(defn hylight [s * [bg "dark"]]
  "Syntax highlight a Hy string. This is nice for use in the repl - put
`(import hyjinx.source [hylight])
(setv repl-output-fn hylight)`
in your .hyrc."
  (let [formatter (TerminalFormatter :bg bg
                                     :stripall True)
        term (shutil.get-terminal-size)]
    (highlight (pformat s :indent 2 :width (- term.columns 5))
               (HyLexer)
               formatter)))

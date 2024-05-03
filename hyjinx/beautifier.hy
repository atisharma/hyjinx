"
A Hy source code pretty-printer.

Should be absorbed into hyjinx.source.

Picolisp does as:

> If an expression is atomic or has a size less or equal to 12, then print it.
> Otherwise, print a left parenthesis, recurse on the CAR, then recurse
> on the elements in the CDR, each on a new line indented by 3 spaces.
> Finally print a right parenthesis.

Here we use 2 spaces for indentation (by default).
And there are a few special cases about when not to break to the next line.
"

;; TODO (can be implemented as methods)
;; * pair off expressions in lists in a let form, (let [a b \n c d] ...)
;; * pair off expressions in dicts {a b  c d}  

(require hyrule [-> ->> unless of])
(require hyjinx.macros [defmethod rest])

(import itertools [batched])
(import hyrule [inc dec])
(import hyjinx [first second last flatten])

(import hy.reader [read_many])
(import hy.models [Object Complex FComponent FString Float Integer Keyword String Symbol])
(import hy.models [Lazy Expression Sequence List Set Dict Tuple])

(import multimethod [multimethod])


(setv SIZE 10)
(setv INDENT_STR "  ")

(setv Atom (| Complex FComponent FString Float Integer String Symbol))
  

;; * Utility functions
;; -----------------------------------

(defn _is-printable [form * size]
  "An expression is a hy Atom or has a flattened length less or equal to `size`."
  (cond
    (isinstance form (| Expression Sequence))
    (<= (len (flatten form)) size)

    (isinstance form String)
    (<= (len form) size)

    (isinstance form Atom)
    True
    
    (isinstance form Object) 
    True

    :else
    (raise (RuntimeError f"Weird object error for form: {form}"))))

(defn _repr [s]
  "Lose the quote."
  (rest (hy.repr s)))

;; * Source code string or Expressions
;; -----------------------------------

(defmethod grind [#^ str source * [size SIZE] #** kwargs]
  "A basic Hy pretty-printer."
  (let [forms (read-many source :skip-shebang True)]
    (grind forms :size size #** kwargs)))

(defmethod grind [#^ Lazy forms #** kwargs]
  "A basic Hy pretty-printer."
  (.join "\n\n"
         (lfor form forms
               (grind form #** kwargs))))
  
(defmethod grind [#^ Expression forms * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  ;; TODO pairwise forms like setv, cond
  (if (_is-printable forms :size size)
      (_repr forms)
      (let [spaces (* INDENT_STR indent)]
        (.join ""
               ["\n" spaces "("
                (.join ""
                       (lfor [ix f] (enumerate forms)
                             ;; keep Keywords with the following form
                             (let [sep (cond (= ix (- (len forms) 1)) ""
                                             (breaks-line f) (+ "\n " spaces " ")
                                             :else " ")]
                               (+ (grind f :indent (inc indent) :size size)
                                  sep))))
                ")"]))))

;; * Atoms, Strings
;; -----------------------------------

(defmethod grind [#^ String s * [indent 0] [size SIZE] #** kwargs]
  "A basic Hy pretty-printer."
  (let [spaces (* INDENT_STR indent)]
    (cond
      ;; short, print as is on the same line
      (_is-printable s :size size)
      (_repr s)
      ;; longer, put on next line
      (< (len s) 75)
      (+ "\n" spaces (_repr s))
      ;; very long, show multiline string
      :else
      (+ "\n" spaces "\"" s "\""))))

(defmethod grind [#^ Keyword kw * [indent 0] #** kwargs]
  "A basic Hy pretty-printer."
  (hy.repr kw))

(defmethod grind [#^ Atom atom #** kwargs]
  "A basic Hy pretty-printer."
  (_repr atom))

;; * Sequences
;; -----------------------------------

(defmethod grind [#^ Dict expr * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (if (_is-printable expr :size size)
      (_repr expr)
      (let [spaces (* INDENT_STR indent)
            kv-sep " "]
        (.join ""
               ["\n" spaces "{"
                (.join (+ "\n " spaces)
                       (lfor [k v] (batched expr 2)
                             (+ (grind k :indent (inc indent) :size size)
                                kv-sep
                                (grind v :indent (inc indent) :size size))))
                "}"]))))
      
(defmethod grind [#^ List forms * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (if (_is-printable forms :size size)
      (_repr forms)
      (let [spaces (* INDENT_STR indent)]
        (.join ""
               ["\n" spaces "["
                (.join (+ "\n " spaces)
                       (lfor f forms
                             (grind f :indent (inc indent) :size size)))
                "]"]))))
  
(defmethod grind [#^ Tuple expr * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (if (_is-printable expr :size size)
      (_repr expr)
      (let [spaces (* INDENT_STR indent)]
        (.join ""
               ["\n" spaces "#("
                (.join "\n"
                       (lfor f forms
                             (+ spaces
                                (grind f :indent (inc indent) :size size))))
                spaces ")"]))))
  
(defmethod grind [#^ Set expr * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (if (_is-printable expr :size size)
      (_repr expr)
      (let [spaces (* INDENT_STR indent)]
        (.join ""
               ["\n" spaces "#{"
                (.join "\n"
                       (lfor f forms
                             (+ spaces
                                (grind f :indent (inc indent) :size size))))
                spaces "}"]))))
                    
;; * Special cases, whether to break a line afterwards
;; -----------------------------------

; defn, def_, fn

;; The default is to break when too long.

(defmethod breaks-line [#^ Object form] True)

(defmethod breaks-line [#^ Symbol symbol]
  "When these symbols are encountered, the next form follows on the same line,
  unless it's too long."
  (cond
    (in (cut (_repr symbol) 3) ["def"])
    False

    (in (_repr symbol) ["if" "let" "filter" "for" "get" "match" "range" "with" "." "join" "keywords"])
    False

    :else
    True))

(defmethod breaks-line [#^ Keyword form] False)

(defmethod breaks-line [#^ Expression forms]
  "Methods / dotted identifiers have a particular form:
      ([. None Symbol])."
  (not (= (_repr (first forms)) ".")))

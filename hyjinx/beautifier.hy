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

;; TODO : pairwise things
;; * pair off expressions in lists in a let form, (let [a b \n c d] ...)
;; * pairwise forms for setv
;; * pairwise forms for cond

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
(setv STR_SIZE 75)
(setv INDENT_STR "  ")

(setv Atom (| Complex FComponent FString Float Integer String Symbol))
  

;; * Tests whether a form is ready to render
;; -----------------------------------------

(defmethod _is-printable [#^ (| Expression Sequence) form * [size SIZE] [str-size STR_SIZE]]
    (<= (len (flatten form)) size))

(defmethod _is-printable [#^ String form * [size SIZE] [str-size STR_SIZE]]
    (<= (len form) str-size))

(defmethod _is-printable [#^ Atom form * [size SIZE] [str-size STR_SIZE]]
    True)

(defmethod _is-printable [#^ Object form * [size SIZE] [str-size STR_SIZE]]
    True)

;; * Render forms to text
;; -----------------------------------------

(defmethod _repr [#^ Object f]
  "Lose the quote."
  (rest (hy.repr f)))

(defmethod _repr [#^ Keyword f]
  "Keep the :."
  (hy.repr f))

(defmethod _repr [#^ Expression forms]
  "Lose the quote."
  (if (and (= (len forms) 3)
           (= (first forms) 'annotate))
    f"#^ {(_repr (last forms))} {(_repr (second forms))} "
    (rest (hy.repr forms))))

;; * Source code string or Expressions
;; -----------------------------------

(defmethod grind [#^ str source * [size SIZE] #** kwargs]
  "A basic Hy pretty-printer."
  (let [forms (read-many source :skip-shebang True)]
    (grind forms :size size #** kwargs)))

(defmethod grind [#^ Lazy forms #** kwargs]
  "A basic Hy pretty-printer."
  (.join "\n\n\n"
         (lfor form forms
               (grind form #** kwargs))))
  
(defmethod grind [#^ Expression forms * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (cond
    (_is-printable forms :size size)
    (_repr forms)

    :else
    (let [spaces (* INDENT_STR indent)]
      (.join ""
             ["("
              (.join ""
                     (lfor [ix f] (enumerate forms)
                           ;; Keep Keywords and so on with the following form.
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
      ;; very long, show multiline string
      :else
      (+ "\"" s "\""))))

(defmethod grind [#^ Keyword kw * [indent 0] #** kwargs]
  "A basic Hy pretty-printer."
  (_repr kw))

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
               ["{"
                (.join (+ "\n " spaces)
                       (lfor [k v] (batched expr 2)
                             (+ (grind k :indent (inc indent) :size size)
                                kv-sep
                                (grind v :indent (inc indent) :size size))))
                "}"]))))
      
(defmethod grind [#^ List forms * [indent 0] [size SIZE] #** kwargs] 
  "A basic Hy pretty-printer."
  (if (_is-printable forms :size size)
      (.join ""
             ["["
              (.join " "
                     (lfor f forms
                           (grind f :indent (inc indent) :size size)))
              "]"])
      (let [spaces (* INDENT_STR indent)]
        (.join ""
               ["["
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
               ["#("
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
               ["#{"
                (.join "\n"
                       (lfor f forms
                             (+ spaces
                                (grind f :indent (inc indent) :size size))))
                spaces "}"]))))
                    
;; * Special cases, whether to break a line afterwards
;; -----------------------------------

(defmethod breaks-line [#^ Object form]
  "The default is to break the expression when it's too long."
  True)

(defmethod breaks-line [#^ Symbol symbol]
  "When these symbols are encountered, the next form follows on the same line,
  unless it's too long."
  (cond
    (in (cut (_repr symbol) 3) ["def"])
    False

    (in (_repr symbol)
        ["import"
         "if" "when" "unless"
         "filter" "of"
         "for" "get" "match" "range"
         "with" "." "join" "keywords"])
    False

    :else
    True))

(defmethod breaks-line [#^ Keyword form] False)

(defmethod breaks-line [#^ Expression forms]
  "Methods / dotted identifiers have a particular form:
      ([. None Symbol])."
  (not (= (_repr (first forms)) ".")))

"
A Hy source code pretty-printer.

Picolisp does as:
> If an expression is atomic or has a size less or equal to 12, then print it.
> Otherwise, print a left parenthesis, recurse on the CAR, then recurse
> on the elements in the CDR, each on a new line indented by 3 spaces.
> Finally print a right parenthesis.

Here we use 2 spaces for indentation (by default).
And there are a few special cases about when not to break to the next line.
"

;; TODO : abstract out form pairing or listing from grind(Expression)

;; TODO : preserve comments

(require hyrule [-> ->> unless of defmain])
(require hyjinx.macros [defmethod rest])

(try
  (import itertools [batched])
  ;; batched was introduced in python 3.12
  (except [ImportError]
    (import itertools [islice])
    (defn batched [iterable n]
      "batched('ABCDEFG', 3) → ABC DEF G))"
      (when (< n 1)
        (raise (ValueError "n must be at least one")))
      (setv it (iter iterable))
      (while (setx batch (tuple (islice it n)))
        (yield batch)))))


(import hyrule [inc dec])
(import hyjinx.lib [first second last flatten slurp])

(import hy.reader [read_many])
(import hy.models [Object Complex FComponent FString Float Integer Keyword String Symbol])
(import hy.models [Lazy Expression Sequence List Set Dict Tuple])

(import multimethod [multimethod])


(setv SIZE 12
      STR_SIZE 75
      INDENT_STR "  ")

(setv Atom (| Complex FComponent FString Float Integer Keyword String Symbol))
  

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

(defn _indent [#^ str indent-str]
  (+ indent-str INDENT_STR))

(defmethod _repr [#^ Object f]
  "The default rendering to string.
  Lose the quote."
  (rest (hy.repr f)))

(defmethod _repr [#^ Keyword f]
  "Keep the : at the front of the keyword."
  (hy.repr f))

(defmethod _repr [#^ List forms]
  "Lose the quote."
  (+ "["
     (.join " "
            (lfor f forms
                  (_repr f)))
     "]"))

(defmethod _repr [#^ Expression forms]
  "Lose the quote, restore type hint and quasiquote."
  (cond

    ;; type hints
    (and (= (len forms) 3)
         (= (first forms) 'annotate))
    f"#^ {(_repr (last forms))} {(_repr (second forms))} "

    ;; quasiquote
    (= (first forms) 'quasiquote)
    (+ "`"
       (.join " "
              (lfor f (rest forms)
                    (_repr f))))

    :else
    (rest (hy.repr forms))))

;; * Special cases
;; -----------------------------------

(defmethod _is-def [#^ Object form]
  "Test if an expression has the first form starting with `'def`."
  False)

(defmethod _is-def [#^ Symbol form]
  "Test if an expression has the first form starting with `'def`."
  (.startswith (_repr form) "def"))
  
(defmethod _is-comprehension [#^ Object form]
  "Test if an expression is a list comprehension."
  False)

(defmethod _is-comprehension [#^ Symbol form]
  "Test if an expression is a list comprehension."
  (in (_repr form) ["lfor" "gfor" "sfor" "dfor"]))

(defmethod _is-paired [#^ Object object #** kwargs]
  "When some symbols are encountered, the next forms go in pairs."
  False)

(defmethod _is-paired [#^ Symbol symbol #** kwargs]
  "When some symbols are encountered (e.g. `cond`), the next forms go in pairs."
  ;; There's no point pairing setv, since the reader expands
  ;; a compound setv statement into individual ones anyway.
  (in symbol ['cond 'setv 'setx]))

(defmethod _takes-paired-list [#^ Object object #** kwargs]
  "When some symbols are encountered, the next form is a paired `List`."
  False)

(defmethod _takes-paired-list [#^ Symbol symbol #** kwargs]
  "When some symbols are encountered, the next form is a paired `List`."
  (in (_repr symbol) ["for" "let" "with"]))

(defmethod _breaks-line [#^ Object form]
  "The default is to break the expression when it's too long."
  True)

(defmethod _breaks-line [#^ Symbol symbol]
  "When these symbols are encountered, the next form follows on the same line,
  unless it's too long."
  (cond
    ;; It's a heuristic, but a reasonable one.
    (in (cut (_repr symbol) 3) ["def"])
    False

    (in symbol
        ['import 'except
         'if 'when 'unless
         'filter 'map 'accumulate 'reduce 'of
         'setv 'setx 'let
         'for 'get 'match 'case 'branch 'range 'while
         'with '. 'join 'keywords])
    False

    :else
    True))

(defmethod _breaks-line [#^ Keyword form]
  False)

(defmethod _breaks-line [#^ Expression forms]
  "Methods / dotted identifiers have a particular form:
      ([. None Symbol])."
  (not (= (first forms) '.)))

;; * The layout engine
;; -----------------------------------

(defmethod _layout [#^ Sequence forms * [indent-str ""] [size SIZE] [pair False] #** kwargs] 
  "The Hy pretty-printer Sequence layout engine.

  This method applies to a sequence of Hy forms, indenting by `indent-str`.
  It will not wrap in parentheses, brackets or the like. That is done in
  the methods specific to various objects."
  (cond

    ;; short and paired
    (and pair
         (_is-printable forms :size size))
    (.join (+ "\n" indent-str)
           (lfor [a b] (batched forms 2)
                 (+ (grind a :indent-str (_indent indent-str) :size size)
                    " "
                    (grind b :indent-str (_indent indent-str) :size size))))

    ;; long, paired, and the first of each pair is short enough
    (and pair
         (all (map (fn [form] (_is-printable form :size 3))
                   (cut forms 0 None 2))))
    ;; then indent the right-hand objects by the longest of the left-hand ones.
    (let [instr (* " " (max (map (fn [f] (len (_repr f))) 
                                (cut forms 0 None 2))))]
      (.join (+ "\n" indent-str)
             (lfor [a b] (batched forms 2)
                 (+ (grind a :indent-str (_indent indent-str) :size size)
                    (cut instr (len (_repr a)) None) " "
                    (grind b :indent-str (+ instr (_indent indent-str)) :size size)))))

    ;; long and paired
    pair
    (.join (+ "\n" indent-str)
           (lfor [a b] (batched forms 2)
                 (+ (grind a :indent-str (_indent indent-str) :size size)
                    "\n\n" (_indent indent-str)
                    (grind b :indent-str (+ "__" (_indent indent-str)) :size size))))

    ;; short and not paired - just print
    (_is-printable forms :size size)
    (.join " "
            (lfor f forms
                    (grind f :indent-str (_indent indent-str) :size size)))

    ;; long and not paired - one on each line
    :else
    (.join (+ "\n" indent-str)
           (lfor f forms
                 (grind f :indent-str (_indent indent-str) :size size)))))

;; * Source code string or Expressions
;; -----------------------------------

(defmethod grind [#^ str source * [size SIZE] #** kwargs]
  "A basic Hy pretty-printer.

  This method is for a source-code string.
  This is probably what you want to use."
  (let [forms (read-many source :skip-shebang True)]
    (grind forms :size size #** kwargs)))

(defmethod grind [#^ Lazy forms #** kwargs]
  "This method is for a lazy sequence of Hy forms."
  (.join "\n\n"
         (lfor form forms
               (grind form #** kwargs))))
  
(defmethod grind [#^ Expression forms * [indent-str ""] [size SIZE] #** kwargs] 
  "This method applies to Hy `Expression` objects
  which are parenthesized sequences of Hy forms."
  (cond

    ;; handle very short forms like type hints and (. None f)
    (_is-printable forms :size 3)
    (_repr forms)

    ;; Expressions with the first form starting with `'def` get a
    ;; preceding line and keep the following two forms with them. Skip
    ;; this if the first three forms (usually includes argument
    ;; signature) are too long or the second form is a list (function
    ;; decorator).
    (and
      (_is-def (first forms))
      (_is-printable (cut forms 3))
      (not (isinstance (get forms 1) List))
      (not (<= (len forms) 3)))
    (+
      "\n" indent-str "("
      (.join " "
             (lfor f (cut forms 3)
                   (_repr f)))
      "\n" (_indent indent-str)
      (.join ""
             (lfor [ix f] (enumerate (cut forms 3 None))
                   ;; Keep Keywords and other special cases with the following form.
                   (let [sep (cond (= ix (- (len forms) 4)) ""
                                   (_breaks-line f) (+ "\n " indent-str " ")
                                   :else " ")]
                     (+ (grind f :indent-str (_indent indent-str) :size size)
                        sep))))
      ")")

    ;; Expressions that are sequence comprehensions keep the following
    ;; two forms with them. Skip this if the first three forms are too
    ;; long.
    (and
      (_is-comprehension (first forms))
      (_is-printable (cut forms 3)))
    (+
      "("
      (.join " "
             (lfor f (cut forms 3)
                   (_repr f)))
      "\n" (_indent indent-str)
      (.join ""
             (lfor [ix f] (enumerate (cut forms 3 None))
                   ;; Keep Keywords and other special cases with the following form.
                   (let [sep (cond (= ix (- (len forms) 4)) ""
                                   (_breaks-line f) (+ "\n " indent-str " ")
                                   :else " ")]
                     (+ (grind f :indent-str (_indent indent-str) :size size)
                        sep))))
      ")")
    
    ;; Forms like `for`, `let` and `with` take a list determining assignments.
    ;; This list should be paired.
    (_takes-paired-list (first forms))
    (let [instr (_indent (* " " (len (first forms))))] 
      (+ "(" (first forms) " "
         ;; List will be paired off
         ;(_indent indent-str)
         (grind (second forms) :indent-str (+ instr indent-str) :size size :pair True)
         "\n" (_indent indent-str)
         ;; rest is processed as normal
         (.join ""
                (lfor [ix f] (enumerate (cut forms 2 None))
                      ;; Keep Keywords and other special cases with the following form.
                      (let [sep (cond (is f (last forms)) ""
                                      (_breaks-line f) (+ "\n " indent-str " ")
                                      :else " ")]
                        (+ (grind f :indent-str (_indent indent-str) :size size) sep))))
         ")"))

    ;; Expressions with few enough forms just get printed
    (_is-printable forms :size size)
    (_repr forms)

    ;; Expressions with `cond` as first form should have the following
    ;; forms go in pairs
    (_is-paired (first forms))
    (+ "(" (first forms) "\n"
       (_indent indent-str)
       (.join (+ "\n\n" (_indent indent-str))
              ;; pair them off
              (lfor [a b] (batched (rest forms) 2)
                    (+ (grind a :indent-str (_indent indent-str) :size size)
                       "\n" (_indent indent-str)
                       (grind b :indent-str (_indent indent-str) :size size))))
       ")")
    
    ;; All other cases follow default indenting rules.
    :else
    (+
      "("
      (.join ""
             (lfor [ix f] (enumerate forms)
                   ;; Keep Keywords and other special cases with the following form.
                   (let [sep (cond (= ix (- (len forms) 1)) ""
                                   (_breaks-line f) (+ "\n " indent-str " ")
                                   :else " ")]
                     (+ (grind f :indent-str (_indent indent-str) :size size)
                        sep))))
      ")")))

;; * Atoms, Strings
;; -----------------------------------

(defmethod grind [#^ String s * [indent-str ""] [size SIZE] #** kwargs]
  "This method applies to Hy `String`."
  (cond
    ;; short, print as is on the same line
    (_is-printable s :size size) (_repr s)
    ;; very long, show multiline string
    :else (+ "\"" (.replace (str s) "\"" r"\"") "\"")))

(defmethod grind [#^ Atom atom #** kwargs]
  "This applies to atomic (non-sequence) Hy forms that are not overridden
  (as `String` is)."
  (_repr atom))

;; * Sequences
;; -----------------------------------

(defmethod grind [#^ Dict expr * [indent-str ""] [size SIZE] #** kwargs] 
  "This is the implementation for `Dict`.
  Key-value pairs are grouped."
  (if (_is-printable expr :size size)
      (_repr expr)
      (+ "{"
         (_layout expr :indent-str (+ " " indent-str) :size size :pair True)
         "}")))
      
(defmethod grind [#^ List expr * [indent-str ""] [size SIZE] [pair False] #** kwargs] 
  "This is the implementation for `List`.
  The `pair` keyword determines whether the list's items presented in pairs."
  (if (_is-printable expr :size size)
      (_repr expr)
      (+ "["
         (_layout expr :indent-str (+ " " indent-str) :size size :pair pair)
         "]"))

  #_(let [items (cond
                  ;; short and paired (for `let` and `with`)
                  (and pair (_is-printable forms :size size))
                  (.join (+ "\n " indent-str)
                         (lfor [a b] (batched forms 2)
                               (+ (grind a :indent-str (_indent indent-str) :size size)
                                  " "
                                  (grind b :indent-str (_indent indent-str) :size size))))
                  ;; long, paired, and the first of each pair is short enough
                  (and pair (all (map (fn [f] (_is-printable f :size 3))
                                      (cut forms 0 None 2))))
                  (let [instr
                        (* " " (max (map (fn [f] (len (_repr f))) 
                                         (cut forms 0 None 2))))]
                    (.join (+ "\n " indent-str)
                           (lfor [a b] (batched forms 2)
                                 (+ (grind a :indent-str (_indent indent-str) :size size)
                                    (cut instr (len (_repr a)) None) " "
                                    (grind b :indent-str (+ instr (_indent indent-str)) :size size)))))
                  ;; long and paired
                  pair
                  (.join (+ "\n " indent-str)
                         (lfor [a b] (batched forms 2)
                               (+ (grind a :indent-str (_indent indent-str) :size size)
                                  "\n" "__" (_indent indent-str)
                                  (grind b :indent-str (+ "__" (_indent indent-str)) :size size))))
                  ;; short and not paired
                  (_is-printable forms :size size)
                  (.join " "
                         (lfor f forms
                               (grind f :indent-str (_indent indent-str) :size size)))
                  ;; long and not paired
                  :else
                  (.join (+ "\n " indent-str)
                         (lfor f forms
                             (grind f :indent-str (_indent indent-str) :size size))))]
        
      (+ "[" items "]")))
    
(defmethod grind [#^ Tuple expr * [indent-str ""] [size SIZE] #** kwargs] 
  "This is the implementation for `Tuple`."
  (if (_is-printable expr :size size)
      (_repr expr)
      (+ "#("
         (_layout expr :indent-str (+ "  " indent-str) :size size :pair False)
         ")")))
  
(defmethod grind [#^ Set expr * [indent-str ""] [size SIZE] #** kwargs] 
  "This is the implementation for `Set`."
  (if (_is-printable expr :size size)
      (_repr expr)
      (+ "#{"
         (_layout expr :indent-str (+ "  " indent-str) :size size :pair False)
         "}")))
      
;; * The cli entrypoints
;; -----------------------------------

(defn grind-file [fname]
  "Pretty-print a hy file."
  (-> fname
      (slurp)
      (grind)
      (print)))

(defn cli-grind-files []
  "Pretty-print hy files from the shell."
  ;; first arg is script name
  (import sys)
  (for [fname (rest sys.argv) :if (.endswith fname ".hy")]
    (grind-file fname)
    (print)))

"
A smorgasbord of useful functions to make Hy more expressive.

It includes functions for:

* module reloading
* function manipulation and composition
* time and date operations
* OS interactions
* string manipulations
* numeric operations
* output and formatting
* list and data structure manipulations
* file I/O and pickling
* JSON handling
* hashing and deterministic uid generation

See individual function docstrings for detailed information.
"

(require hyrule [unless -> ->> as->]
         hyjinx.macros *)

(require hyjinx.macros [rest lmap])

(import functools *
        itertools *
        cytoolz [first second last drop partition identity]
        hyrule [flatten pformat pp :as hyrule-pp])

(import os re unicodedata)

(import pathlib [Path])


;; * Modules
;; ----------------------------------------------------

(defn mreload [#* modules]
  "Reload a list of modules in order."
  (lmap hy.I.importlib.reload modules))

;; * Functions
;; ----------------------------------------------------

(defn named-partial [f #* args #** kwargs]
  "Just functools.partial, but with a new function name set."
  (setv f-partial (partial f #* args #** kwargs))
  (setv f-partial.__name__ (.join "_" [(name f) #*(map str args) #*(map str (flatten (.items kwargs)))]))
  f-partial)

(defn compose [#* funcs]
  "Function composition, f ∘ g.
  E.g., after
  (setv fg (compose f g)).
  then
  (f (g x)) is equivalent to (fg x).
  Arguments must have compatible input/output matchings."
  (reduce
    (fn [f g]
      (fn [#* args #** kwargs] (f (g #* args #** kwargs))))
    funcs))

;; * Time and date
;; ----------------------------------------------------

(defn timestamp [l]
  "Convert list of timestamps to a list of datetime objects."
  (->> l
    (map (fn [t] (.fromtimestamp hy.I.datetime.datetime t))) 
    (list)))

(defn days-ago [n]
  "Return datetime for n days ago."
  (.date (- (hy.I.datetime.datetime.today)
            (hy.I.datetime.timedelta :days n))))

(defn yesterday []
  "Return datetime for yesterday."
  (days-ago 1))

(defn tomorrow []
  "Return datetime for tomorrow."
  (days-ago -1))

;; * OS
;; ----------------------------------------------------

(defn mkdir [d]
  (.mkdir (Path d)
          :parents True
          :exist-ok True))  

(defn ! [#* args]
  "Return the output of running a command in a shell."
  (. (hy.I.subprocess.run (.join " " args)
                          :shell True
                          :capture-output True
                          :encoding "utf-8")
     stdout))

(defn pwd [path]
  "Alis to get working directory."
  (os.getcwd))

(defn cd [path]
  "Change working directory."
  (os.chdir path))

(defn ls [[path None]]
  "Directory listing."
  (os.listdir path))

(defn grepp [regex lines * [line-nos False]]
  "Get Regular Expression in python.
  Return items in a string or iterable of strings (lines) that match a regular expression."
  (let [rx (re.compile regex)
        ls (if (isinstance lines str)
               (.split lines "\n")
               lines)]
    (if line-nos
      (lfor [ln l] (enumerate ls) :if (re.search regex l) f"{ln :04d}: {l}")
      (lfor l ls :if (re.search regex l) l))))

(defn shell [[shell "bash"] #* args]
  "Run an interactive shell as a subprocess.
  Usually, you could instead suspend Hy with ctrl-z."
  (hy.I.subprocess.run (.join " " [shell #* args])
                       :shell False
                       :stdin hy.I.sys.stdin
                       :stdout hy.I.sys.stdout
                       :encoding "utf-8"))

(defn edit [fname]
  "Quick and dirty edit. Use system editor if defined, else vi."
  (let [editor (os.getenv "EDITOR" "vi")]
    (try
      (hy.I.subprocess.run [editor fname] :check True))))

;; * Strings
;; ----------------------------------------------------

(defn get-numeric [text]
  "Extract a flat list of numbers by appearance."
  (lfor n (re.findall "[-]*[0-9]+[.]*[0-9]*" (str text))
        (float n)))

(defn sstrip [s]
  "Strip surrounding whitespace, quotes, '.',
  force to lowercase, remove 'the' from start of line."
  (re.sub r"^[tT]he " ""
          (-> s
              (.strip "\n\t .\"'`")
              (.lower))))

(defn similar [s1 s2 [threshold 0.8]] ; -> bool
  "Two words are heuristically similar, based on Jaro-Winkler algorithm."
  (import jaro)
  (let [cs1 (sstrip (str s1))
        cs2 (sstrip (str s2))
        score (jaro.jaro-winkler-metric cs1 cs2)]
    (> score threshold)))

(defn decimal-align [x lpad]
 "Return string aligned on decimal."
 (if (and x
          (not (hy.I.math.isnan x))
          (in "." (str x)))
  (.join "." [f"{(int x) : {lpad},.0f}" (second (.split f"{x}" "."))])
  (str x)))

(defn unicode-search [s]
  "Search for unicode characters with s in the name."
  (for [i (range 0x10000)]
    (let [char (chr i)
          category (unicodedata.category char)
          name (unicodedata.name char "")]
      (when (or (in (.lower s) (.lower name)))
        (print f"U+{i :04x} {category} {name} {char}")))))

;; * Numeric
;; ----------------------------------------------------

(defn as-float [x]
  "Cast scalar to float, but use scalar NaN if it fails.
  Beware of passing lists."
  (try
    (float x)
    (except [] NaN)))

(defn sign [x]
  "+1 for x>=0 (if x is positive semidefinite), -1 for x<0 (negative definite)."
  (hy.I.math.copysign 1 x))

(defn round-to [x y]
  "Round x to precision y, towards zero."
  (* y (int (/ x y))))

(defn pos? [x]
  "x is positive (including zero)."
  (>= x 0))
  
(defn neg? [x]
  "x is negative definite (excludes zero)."
  (< x 0))

(defn zero? [x]
  "x is exactly (integer) zero."
  (= x 0))

(defn number? [x]
  "x has a Number type."
  (isinstance x hy.I.numbers.Number))
  
(defn dice [n]
  "True 1/n of the time."
  (not (hy.I.random.randint 0 (- n 1))))

(defn prod [l]
  "The product of the elements in l."
  (reduce hy.I.operator.mul l))

;; * Output
;; ----------------------------------------------------

(defn pp [x #* args #** kwargs]
  "Pretty-print with better defaults."
  (let [term (hy.I.shutil.get-terminal-size)]
    (hyrule-pp x :indent 2 :width (- term.columns 5) #* args #** kwargs)))

(defn hash-color [s]
  "A hex RGB colour mapped from a string."
  (import pansi [ansi])
  (let [S (.upper s)
        i (-> (.upper s)
              (.encode "utf-8")
              (hy.I.hashlib.md5)
              (.hexdigest)
              (int 16)
              (% (** 2 24)))]
    (get ansi.rgb (.replace f"{i :#x}" "0x" "#"))))

;; * Manipulations on lists and other basic data structures
;; ----------------------------------------------------

(defn sieve [xs]
  (filter None xs))

(defn shift [l]
  (.pop l 0))

(defn take [n it]
  "Return first n items of iterable as a list."
  ;; see more-itertools
  (list (islice it n)))

(defn get-in [coll #* args]
  "A recursive get that returns None in case of missing keys."
  (if (and (isinstance coll dict)
           args
           (in (first args) coll))
    (get-in (get coll (first args)) #* (rest args))
    (if args
        None
        coll)))

;; * Files
;; ----------------------------------------------------

(defn config [config-file]
  "Get values in a toml file like a hashmap."
  ;; requires python 3.11
  (if (os.path.isfile config-file)
    (-> config-file
        (slurp)
        (hy.I.tomllib.loads))
    (raise (FileNotFoundError config-file))))

(defn slurp [fname #** kwargs]
  "Read a file and return as a string.
  kwargs can include mode, encoding and buffering, and will be passed
  to open()."
  (let [f (if (:encoding kwargs None)
              hy.I.codecs.open
              open)]
    (with [o (f fname #** kwargs)]
      (o.read))))

(defn spit [fname content * [mode "w"] #** kwargs]
  "Write content as file fname.
  kwargs can include mode, encoding and buffering, and will be passed
  to open()."
  (let [f (if (:encoding kwargs None)
              hy.I.codecs.open
              open)]
    (with [o (f fname mode #** kwargs)]
      (o.write content))))

;; * pickling
;; ----------------------------------------------------

(defn pload [fname]
  "Read a pickle file. None if it doesn't exist."
  (let [path (Path fname)]
    (when (path.exists)
      (with [f (open fname :mode "rb")]
        (hy.I.pickle.load f))))) 

(defn psave [obj fname * [protocol -1]]
  "Write an object as a pickle file."
  (with [f (open fname :mode "wb")]
    (hy.I.pickle.dump obj f :protocol protocol)))

;; * JSON
;; ----------------------------------------------------

(defn extract-json [text]
  "Extract anything vaguely like { ... }."
  (let [result (re.search "{.*}" text re.DOTALL)]
    (if (and result (result.group))
        (try
          (hy.I.json.loads (result.group))
          (except [err [hy.I.json.JSONDecodeError]]
            {}))
      {})))

(defn jload [fname * [encoding "utf-8"]]
  "Read a json file. None if it doesn't exist."
  (let [path (Path fname)]
    (when (path.exists)
      (with [f (open fname
                     :mode "r"
                     :encoding encoding)]
        (hy.I.json.load f)))))

(defn jsave [obj fname * [encoding "utf-8"]]
  "Write an object as a json file."
  (with [f (open fname
                 :mode "w"
                 :encoding encoding)]
    (hy.I.json.dump obj f :indent 4)))

(defn jappend [record fname * [encoding "utf-8"]]
 "Append / write a dict to a file as json.
  If the file does not exist, initialise a file with the record.
  If the file exists, append to it.
  Cobbled together from https://stackoverflow.com/a/31224105
  it overwrites the closing ']' with the new record + a new ']'.
  POSIX expects a trailing newline. Assumes utf-8."
  (if (os.path.isfile fname)
    (with [f (open fname :mode "r+" :encoding encoding)]
      (.seek f 0 os.SEEK_END)
      (.seek f (- (.tell f) 2))
      (.write f (.format ",\n{}]\n" (hy.I.json.dumps record :indent 4))))
    (with [f (open fname :mode "w" :encoding encoding)]
      (.write f (.format "[\n{}]\n" (hy.I.json.dumps record :indent 4))))))

;;; Hashing, id and password functions
;;; -----------------------------------------------------------------------------

(defn hash-id [s]
  "Hex digest of sha1 hash of string."
  (-> (s.encode "utf-8")
      (hy.I.hashlib.sha1)
      (.hexdigest)))

(defn short-id [x]
  "First 6 chars of hash-id."
  (cut (hash-id x) 6))

"
A smorgasbord of useful functions to make Hy more expressive.

It includes functions for:

* module reloading
* function manipulation and composition
* time and date operations
* OS interaction
* string manipulations
* numeric operations
* output and formatting
* list and data structure manipulations
* file serialization (pickling, JSON)
* hashing and deterministic uid generation

See individual function docstrings for detailed information.
"

(require hyrule [unless -> ->> as->]
         hyjinx.macros *)

(require hyjinx.macros [lmap])

(import hy [mangle])
(import functools *
        itertools *
        toolz.itertoolz [rest]
        toolz [take first second last drop partition identity]
        hyrule [flatten pformat pp :as hyrule-pp])

(import asyncio)
(import os re unicodedata)
(import sys [stdout])
(import pathlib [Path])

(import ansi-escapes [ansiEscapes])


;; * Modules
;; ----------------------------------------------------

(defn mreload [#* modules]
  "Reload a list of modules in order."
  (lmap hy.I.importlib.reload modules))


;; * async and coroutines
;; ----------------------------------------------------

(defn sync-await [coroutine]
  "Call a coroutine from inside a synchronous function,
  itself called from inside the async event loop.
  See also `asyncio.to-thread`."
  (import asyncio)
  (asyncio.run-coroutine-threadsafe
    coroutine
    (asyncio.get-event-loop)))

;; FIXME doesn't work
(defn :async coroutine [f [process False] #* args #** kwargs]
  "Decorator to run a synchronous function in an executor.
  Defaults to `ThreadPoolExecutor`, more suitable for IO-bound tasks,
  otherwise when `process` is `True`, uses `ProcessPoolExecutor`, for CPU-bound tasks.
  Remember that the GIL exists."
  (import asyncio [get-event-loop])
  (import concurrent.futures [ProcessPoolExecutor ThreadPoolExecutor])
  (with [executor (if process (ProcessPoolExecutor) (ThreadPoolExecutor))]
    (await (.run-in-executor (get-event-loop) executor f #* args #** kwargs)))) 


;; * Functions
;; ----------------------------------------------------

(defn named-partial [f #* args #** kwargs]
  "Just functools.partial, but with a new function name set."
  (setv f-partial (partial f #* args #** kwargs))
  ;; FIXME: assumes everything is expressible as a string
  (let [arg-names (map (fn [x] (mangle (str x)))
                       args)
        kwarg-names (map (fn [item] (mangle f"{(str (first item))}={(str (second item))}"))
                         (.items kwargs))
        new-name (.join "_" [f.__name__ #* arg-names #* kwarg-names])]
    (setv f-partial.__name__ new-name)
    f-partial))

(defn compose [#* funcs]
  "Function composition, f ∘ g.
  E.g., after
  `(setv fg (compose f g))`
  then
  `(f (g x))` is equivalent to `(fg x)`.
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

(defn now []
  "Current timestamp in isoformat (as text) with timezone."
  (-> hy.I.datetime.timezone.utc
      (hy.I.datetime.datetime.now)
      (.astimezone)
      (.isoformat)))


;; * OS
;; ----------------------------------------------------

(defn mkdir [d]
  (.mkdir (Path d)
          :parents True
          :exist-ok True))  

(defn ! [#* args]
  "Return the output of running a command in a shell."
  (. (hy.I.subprocess.run args
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
  "Search for regular expressions in lines of text.
  Return items in a string or iterable of strings (lines) that match a regular expression.
  If `line-nos` is True, return the matching lines with their line numbers.
  The default is False, which returns only the matching lines."
  (let [rx (re.compile regex)
        line-list (if (isinstance lines str)
                    (.split lines "\n")
                    lines)]
    (if line-nos
      (lfor [index line] (enumerate line-list) :if (re.search regex line) f"{index :04d}: {line}")
      (lfor line line-list :if (re.search regex line) line))))

(defn shell [[shell "bash"] #* args]
  "Run an interactive shell as a subprocess.
  Usually, you would instead suspend Hy with ctrl-z."
  (hy.I.subprocess.run (.join " " [shell #* args])
                       :shell False
                       :stdin hy.I.sys.stdin
                       :stdout hy.I.sys.stdout
                       :encoding "utf-8"))

(defn username []
  "The user's username, on unix or Windows."
  (os.environ.get "USER" (os.environ.get "USERNAME")))


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
  "Determine if two words are heuristically similar, based on the Jaro-Winkler algorithm.
  The `threshold` parameter specifies the minimum similarity score to consider
  the words as similar. The default is 0.8.
  The comparison is case-insensitive."
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
  "Search for Unicode characters whose names match the given substring `s`.
  Iterates over the Unicode code points from 0 to 0x10000.
  Prints the character's code point, category, name, and the character itself."
  (for [i (range 0x10000)]
    (let [char (chr i)
          name (unicodedata.name char "")
          category (unicodedata.category char)]
      (when (in (.lower s) (.lower name))
        (print f"U+{i :04x} {category} {name} {char}")))))

(defn is-url [url]
  "True if a plausible url."
  (import urllib.parse [urlparse])
  (let [result (urlparse url)]
    (all [result.scheme result.netloc])))


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
    (get ansi.rgb (.replace f"{i :#08x}" "0x" "#"))))

(defn progress [string #** kwargs]
  "Simple multi-line progress output.
  Splits string on newline, substitutes in kwargs with its `.format` method."
  (let [cols (- (. (hy.I.shutil.get-terminal-size) columns) 10)
        strings (.split string "\n")]
    (for [s strings]
      (stdout.write ansiEscapes.eraseLine)
      (-> (.format s #** kwargs)
          (.replace "\n"  "\\n")
          (.strip)
          (cut cols)
          (print))
      (stdout.flush))
    (stdout.write (ansiEscapes.cursorUp (len strings)))
    (stdout.flush)))


;; * Manipulations on lists and other basic data structures
;; ----------------------------------------------------

(defn sieve [xs]
  (filter None xs))

(defn shift [l]
  (.pop l 0))

(defn get-in [coll #* args]
  "A recursive get that returns None in case of missing keys."
  (if (and (isinstance coll dict)
           args
           (in (first args) coll))
    (get-in (get coll (first args)) #* (rest args))
    (if args
        None
        coll)))

(defn group [iterable n]
  "Collect data into chunks of length `n`."
  ;; (group "ABCDEFG" 3) --> ABC DEF G
  (let [iterator (iter iterable)]
    (iter (fn [] (tuple (islice iterator n))) #())))


;; * Files
;; ----------------------------------------------------

(defn config [config-file]
  "Get values in a toml file like a hashmap."
  ;; requires python 3.11
  (-> config-file
      (slurp)
      (hy.I.tomllib.loads)))

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

(defn template [fname #** kwargs]
  "Load a string template from a file and substitute in the named kwargs
  using python's `str.format` method."
  (-> fname
      (slurp)
      (.format #** kwargs)))

(defmethod blog [fname #^ str s]
  "(Very) basic logging to a file.
  Opens and closes the file at each write."
  (spit fname (+ (now) " " s "\n") :mode "a"))
  
(defmethod blog [fname #^ Exception e]
  "(Very) basic logging to a file.
  Opens and closes the file at each write."
  (spit fname (+ (now) " Error: " (repr e) "\n") :mode "a"))
  
(defmethod blog [fname s #^ Exception e]
  "(Very) basic logging to a file.
  Opens and closes the file at each write."
  (spit fname (+ (now) f" {(str s)}; Error: " (repr e) "\n") :mode "a"))
  

;; * pickling
;; ----------------------------------------------------

(defn pload [fname]
  "Read a pickle file. None if it doesn't exist."
  (let [path (Path fname)]
    (when (path.exists)
      (with [f (open fname :mode "rb")]
        (hy.I.pickle.load f))))) 

(defn psave [obj fname * [protocol -1]]
  "Write an object as a pickle file.
  Defaults to the highest available protocol."
  (with [f (open fname :mode "wb")]
    (hy.I.pickle.dump obj f :protocol protocol)))


;; * JSON
;; ----------------------------------------------------

;; TODO json-repair seems better; try it out
(defn extract-json [text]
  "Extract anything vaguely like { ... } or [ ... ] from a string."
  (let [jobj (re.search "{.*}" text re.DOTALL)
        jarr (re.search r"\[.*\]" text re.DOTALL)]
    (cond
      (and jobj (jobj.group))
      (try
        (hy.I.json.loads (jobj.group))
        (except [err [hy.I.json.JSONDecodeError]]
          {}))

      (and jarr (jarr.group))
      (try
        (hy.I.json.loads (jarr.group))
        (except [err [hy.I.json.JSONDecodeError]]
          []))

      :else
      {})))

(defn jload [fname * [encoding "utf-8"]]
  "Read a json file. Return None if it doesn't exist."
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
  (if (Path.is-file (Path fname))
    (with [f (open fname :mode "r+" :encoding encoding)]
      (.seek f 0 os.SEEK_END)
      (.seek f (- (.tell f) 2))
      (.write f (.format ",\n{}]\n" (hy.I.json.dumps record :indent 4))))
    (with [f (open fname :mode "w" :encoding encoding)]
      (.write f (.format "[\n{}]\n" (hy.I.json.dumps record :indent 4))))))

(defn filetype [fname]
  "Guess the file type from various cues.
  Return doc metadata including mime type and extension."
  (import magic)
  (let [mime (magic.from-file fname :mime True)
        [mime-type mime-subtype] (if (in "/" mime)
                                   (.split mime "/" 1)
                                   mime)
        ;; could use Path.suffix
        extension (if (in "." fname)
                   (last (.split fname "."))
                   None)]
    {"mime_type" mime-type
     "mime_subtype" mime-subtype
     "mime" mime
     "extension" extension
     "type" "file"
     "source" fname}))

(defn filenames [directory
                 [ignored-dirs [".git" ".svn" ".cvs" ; vcs
                                ".venv" "venv" "__pycache__" "dist" "build" ".*egg-info" ; python
                                ".cache" ".zk"]]]
  "Create a generator of all files under a directory (recursively).
  Ignore specified sub-directories."
  (flatten
    (gfor [dirpath subdirs filenames] (os.walk directory)
          ;; remove the appropriate directories from the tree
          :do (for [pattern ignored-dirs
                    subdir (.copy subdirs)
                    :if (re.fullmatch pattern subdir)]
                (.remove subdirs subdir))
          ;; for what remains, the path is returned
          (gfor fname filenames
                (os.path.join dirpath fname)))))


;; * Hashing, id and password functions
;; -----------------------------------------------------------------------------

(defn hash-id [s]
  "Hex digest of sha1 hash of string."
  (-> (s.encode "utf-8")
      (hy.I.hashlib.sha1)
      (.hexdigest)))

(defn short-id [x [n 6]]
  "First n (6) chars of hash-id."
  (cut (hash-id x) n))

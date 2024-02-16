"
A smorgasbord of useful functions.
"

(require hyrule [unless -> ->> as->]
         hyjinx.macros *)

(require hyjinx.macros [rest])

(import functools *
        itertools *
        cytoolz [first second last partition identity]
        hyrule [flatten pformat pp :as hyrule-pp])

(import json
        math
        operator
        os
        re
        shutil
        subprocess
        sys)

(import pathlib [Path])
(import datetime [datetime])

(import random [randint])
(import hashlib [sha1 md5])


;; * Functions
;; ----------------------------------------------------

(defn named-partial [f #* args #** kwargs]
  "functools.partial, but with a new function name set."
  (setv f-partial (partial f #* args #** kwargs))
  (setv f-partial.__name__ (.join "_" [(name f) #*(map str args) #*(map str (flatten (.items kwargs)))]))
  f-partial)

;; * Time and date
;; ----------------------------------------------------

(defn timestamp [l]
  "Convert list of timestamps to a list of datetime objects."
  (->> l
    (map (fn [t] (.fromtimestamp datetime t))) 
    (list)))

;; * OS
;; ----------------------------------------------------

(defn mkdir [d]
  (.mkdir (Path d)
          :parents True
          :exist-ok True))  

(defn ! [#* args]
  "Return the output of running a command in a shell."
  (. (subprocess.run (.join " " args)
                     :shell True
                     :capture-output True
                     :encoding "utf-8")
     stdout))

(defn cd [path]
  "Change directory."
  (os.chdir path))

(defn ls [[path None]]
  "Directory listing."
  (os.listdir path))

(defn re-grep [regex lines * [line-nos False]]
  "Return items in a string or iterable of strings (lines) that match a regular expression."
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
  (subprocess.run (.join " " [shell #* args])
                  :shell False
                  :stdin sys.stdin
                  :stdout sys.stdout
                  :encoding "utf-8"))

(defn edit [fname]
  "Quick and dirty edit. Use system editor if defined, else vi."
  (let [editor (os.getenv "EDITOR" "vi")]
    (try
      (subprocess.run [editor fname] :check True))))

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
          (not (nan? x))
          (in "." (str x)))
  (.join "." [f"{(int x) : {lpad},.0f}" (second (.split f"{x}" "."))])
  (str x)))

;; * Numeric
;; ----------------------------------------------------

(defn sign [x]
  "+1 for x>=0 (if x is positive semidefinite), -1 for x<0 (negative definite)."
  (math.copysign 1 x))

(defn round-to [x y]
  "Round x to precision y, towards zero."
  (* y (int (/ x y))))

(defn pos? [x]
  "x is positive (including zero)."
  (>= x 0))
  
(defn neg? [x]
  "x is strictly negative (excludes zero)."
  (< x 0))

(defn zero? [x]
  "x is exactly (integer) zero."
  (= x 0))
  
(defn dice [n]
  "True 1/n of the time."
  (not (randint 0 (- n 1))))

(defn prod [l]
  "The product of the elements in l."
  (reduce operator.mul l))

;; * Output
;; ----------------------------------------------------

(defn pp [x #* args #** kwargs]
  "Pretty-print with better defaults."
  (let [term (shutil.get-terminal-size)]
    (hyrule-pp x :indent 2 :width (- term.columns 5) #* args #** kwargs)))

(defn hash-color [s]
  "A hex RGB colour mapped from a string."
  (import pansi [ansi])
  (let [S (.upper s)
        i (-> (.upper s)
              (.encode "utf-8")
              (md5)
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
  (import os)
  (import tomllib) ;; requires python 3.11
  (unless (os.path.isfile config-file)
    (raise (FileNotFoundError config-file)))
  (-> config-file
      (slurp)
      (tomllib.loads)))

(defn slurp [fname [mode None] [encoding None] [buffering None]]
  "Read a file and return as a string."
  (setv f open)
  (when encoding
    (import codecs)
    (setv f codecs.open))
  (with [o (f fname #** (dict (+
                               (if (is mode None)      [] [#("mode" mode)])
                               (if (is encoding None)  [] [#("encoding" encoding)])
                               (if (is buffering None) [] [#("buffering" buffering)]))))]
    (o.read)))

(defn spit [fname content [mode "w"] [encoding None] [buffering None]]
  "Write content as file fname."
  (setv f open)
  (when encoding
    (import codecs)
    (setv f codecs.open))
  (with [o (f fname mode #** (dict (+
                                    (if (is encoding None)  [] [#("encoding" encoding)])
                                    (if (is buffering None) [] [#("buffering" buffering)]))))]
    (o.write content)))

;; * JSON
;; ----------------------------------------------------

(defn extract-json [text]
  "Extract anything vaguely like { ... }."
  (let [result (re.search "{.*}" text re.DOTALL)]
    (if (and result (result.group))
        (try
          (json.loads (result.group))
          (except [err [json.JSONDecodeError]]
            {}))
      {})))

(defn jload [fname * [encoding "utf-8"]]
  "Read a json file. None if it doesn't exist."
  (let [path (Path fname)]
    (when (path.exists)
      (with [f (open fname
                     :mode "r"
                     :encoding encoding)]
        (json.load f)))))

(defn jsave [obj fname * [encoding "utf-8"]]
  "Write an object as a json file."
  (with [f (open fname
                 :mode "w"
                 :encoding encoding)]
    (json.dump obj f :indent 4)))

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
      (.write f (.format ",\n{}]\n" (json.dumps record :indent 4))))
    (with [f (open fname :mode "w" :encoding encoding)]
      (.write f (.format "[\n{}]\n" (json.dumps record :indent 4))))))

;;; Hashing, id and password functions
;;; -----------------------------------------------------------------------------

(defn hash-id [s]
  "Hex digest of sha1 hash of string."
  (-> (s.encode "utf-8")
      (sha1)
      (.hexdigest)))

(defn short-id [x]
  "First 6 chars of hash-id."
  (cut (hash-id x) 6))

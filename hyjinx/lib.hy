(require hyrule [unless -> ->> as->]
         hyjinx.macros *)

(import functools *
        itertools *
        hyrule [flatten]
        cytoolz [first second partition identity])

(import os subprocess)
(import re)
(import json)
(import pathlib [Path])
(import datetime [datetime])

(import hashlib [sha1])


;; * Functions
;; ----------------------------------------------------

(defn partial [f #* args #** kwargs]
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

(defn mksubdir [d]
  (.mkdir (Path (.join "/" [path d]))
          :parents True
          :exist-ok True))  

(defn ! [#* args]
  (. (subprocess.run (.join " " args)
                     :shell True
                     :capture-output True
                     :encoding "utf-8")
     stdout))

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
  (setv f open)
  (when encoding
    (import codecs)
    (setv f codecs.open))
  (with [o (f fname #** (dict (+
                               (if (is mode None)      [] [#("mode" mode)])
                               (if (is encoding None)  [] [#("encoding" encoding)])
                               (if (is buffering None) [] [#("buffering" buffering)]))))]
    (o.read)))

(defn barf [fname content [mode "w"] [encoding None] [buffering None]]
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

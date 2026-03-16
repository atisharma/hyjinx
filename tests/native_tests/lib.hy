"
Tests for hyjinx.lib — the utility function library.
"

(import json)
(import os)
(import tempfile)
(import pathlib [Path])

(import hyjinx.lib)
(import hyjinx.result [ok err unwrap])


;; ── String helpers ────────────────────────────────────────────────────────────

(defn test-sstrip-strips-whitespace-and-quotes []
  (assert (= (hyjinx.lib.sstrip "  Hello.  ") "hello")))

(defn test-sstrip-removes-the-prefix []
  (assert (= (hyjinx.lib.sstrip "The quick brown fox") "quick brown fox")))

(defn test-grepp-returns-matching-lines []
  (let [lines ["apple" "banana" "apricot" "cherry"]
        result (hyjinx.lib.grepp "^a" lines)]
    (assert (= result ["apple" "apricot"]))))

(defn test-grepp-on-string []
  (let [text "line1\nline2\napple"
        result (hyjinx.lib.grepp "apple" text)]
    (assert (= result ["apple"]))))

(defn test-grepp-line-nos []
  (let [lines ["one" "two" "three"]
        result (hyjinx.lib.grepp "two" lines :line_nos True)]
    (assert (= (len result) 1))
    (assert (in "0001" (get result 0)))))

(defn test-is-url-valid []
  (assert (hyjinx.lib.is-url "https://example.com"))
  (assert (hyjinx.lib.is-url "http://foo.bar/baz")))

(defn test-is-url-invalid []
  (assert (not (hyjinx.lib.is-url "not-a-url")))
  (assert (not (hyjinx.lib.is-url ""))))

(defn test-get-numeric-extracts-numbers []
  (let [nums (hyjinx.lib.get_numeric "price: 3.14 and -42 items")]
    (assert (in 3.14 nums))
    (assert (in -42.0 nums))))


;; ── Numeric helpers ───────────────────────────────────────────────────────────

(defn test-as-float-valid []
  (assert (< (abs (- (hyjinx.lib.as_float "3.14") 3.14)) 0.001))
  (assert (= (hyjinx.lib.as_float 42) 42.0)))

(defn test-as-float-invalid-returns-nan []
  (import math)
  (let [result (hyjinx.lib.as_float "banana")]
    (assert (math.isnan result))))

(defn test-isnumeric-true []
  (assert (hyjinx.lib.isnumeric "3.14"))
  (assert (hyjinx.lib.isnumeric 0)))

(defn test-isnumeric-false []
  (assert (not (hyjinx.lib.isnumeric "abc")))
  (assert (not (hyjinx.lib.isnumeric None))))

(defn test-sign-positive []
  (assert (= (hyjinx.lib.sign 5) 1.0))
  (assert (= (hyjinx.lib.sign 0) 1.0)))

(defn test-sign-negative []
  (assert (= (hyjinx.lib.sign -3) -1.0)))

(defn test-round-to []
  (assert (= (hyjinx.lib.round_to 17 5) 15))
  (assert (= (hyjinx.lib.round_to 23 10) 20)))

(defn test-pos-neg-zero []
  (assert (hyjinx.lib.pos? 1))
  (assert (hyjinx.lib.pos? 0))
  (assert (hyjinx.lib.neg? -1))
  (assert (hyjinx.lib.zero? 0))
  (assert (not (hyjinx.lib.zero? 1))))


;; ── Data structure helpers ────────────────────────────────────────────────────

(defn test-get-in-nested []
  (let [d {"a" {"b" {"c" 42}}}]
    (assert (= (hyjinx.lib.get_in d "a" "b" "c") 42))))

(defn test-get-in-missing-returns-none []
  (let [d {"a" {"b" 1}}]
    (assert (is (hyjinx.lib.get_in d "a" "x" "y") None))))

(defn test-get-in-empty-path []
  (let [d {"a" 1}]
    (assert (= (hyjinx.lib.get_in d) d))))

(defn test-sieve-removes-none []
  (let [result (list (hyjinx.lib.sieve [1 None 2 None 3]))]
    (assert (= result [1 2 3]))))

(defn test-group []
  (let [groups (list (hyjinx.lib.group [1 2 3 4 5] 2))]
    (assert (= groups [#(1 2) #(3 4) #(5)]))))

(defn test-short-id-deterministic []
  (let [a (hyjinx.lib.short_id "hello")
        b (hyjinx.lib.short_id "hello")]
    (assert (= a b))
    (assert (= (len a) 6))))

(defn test-hash-id-is-hex []
  (let [h (hyjinx.lib.hash_id "test")]
    (assert (all (lfor c h (in c "0123456789abcdef"))))))


;; ── File I/O: slurp / spit ────────────────────────────────────────────────────

(defn test-roundtrip [tmp-path]
  (let [f (str (Path tmp-path "test.txt"))]
    (hyjinx.lib.spit f "hello world")
    (assert (= (hyjinx.lib.slurp f) "hello world"))))

(defn test-spit-append-mode [tmp-path]
  (let [f (str (Path tmp-path "log.txt"))]
    (hyjinx.lib.spit f "line1\n")
    (hyjinx.lib.spit f "line2\n" :mode "a")
    (let [content (hyjinx.lib.slurp f)]
      (assert (in "line1" content))
      (assert (in "line2" content)))))

(defn test-slurp-missing-raises [tmp-path]
  (try
    (hyjinx.lib.slurp (str (Path tmp-path "nonexistent.txt")))
    (except [e FileNotFoundError]
      (assert True))))


;; ── slurp-result (Result integration) ────────────────────────────────────────

(defn test-slurp-result-ok [tmp-path]
  (let [f (Path tmp-path "data.txt")]
    (f.write_text "content here")
    (let [r (hyjinx.lib.slurp_result (str f))]
      (assert (get r "ok"))
      (assert (= (unwrap r) "content here")))))

(defn test-slurp-result-err [tmp-path]
  (let [r (hyjinx.lib.slurp_result (str (Path tmp-path "ghost.txt")))]
    (assert (not (get r "ok")))
    (assert (= (get r "error" "type") "file-not-found"))))


;; ── JSON I/O: jload / jsave ───────────────────────────────────────────────────

(defn test-jload-jsave-roundtrip [tmp-path]
  (let [f (str (Path tmp-path "data.json"))
        obj {"key" "value" "num" 42}]
    (hyjinx.lib.jsave obj f)
    (let [loaded (hyjinx.lib.jload f)]
      (assert (= loaded obj)))))

(defn test-jload-missing-returns-none [tmp-path]
  (let [result (hyjinx.lib.jload (str (Path tmp-path "nope.json")))]
    (assert (is result None))))


;; ── jload-result (Result integration) ────────────────────────────────────────

(defn test-jload-result-ok [tmp-path]
  (let [f (Path tmp-path "data.json")]
    (f.write_text "{\"x\": 1}")
    (let [r (hyjinx.lib.jload_result (str f))]
      (assert (get r "ok"))
      (assert (= (unwrap r) {"x" 1})))))

(defn test-jload-result-file-not-found [tmp-path]
  (let [r (hyjinx.lib.jload_result (str (Path tmp-path "missing.json")))]
    (assert (not (get r "ok")))
    (assert (= (get r "error" "type") "file-not-found"))))

(defn test-jload-result-invalid-json [tmp-path]
  (let [f (Path tmp-path "bad.json")]
    (f.write_text "this is not json {{{")
    (let [r (hyjinx.lib.jload_result (str f))]
      (assert (not (get r "ok")))
      (assert (= (get r "error" "type") "json-decode-error")))))


;; ── extract-json (refactored to return Result) ────────────────────────────────

(defn test-extract-json-ok-object []
  (let [r (hyjinx.lib.extract_json "prefix {\"key\": \"value\"} suffix")]
    (assert (get r "ok"))
    (assert (= (unwrap r) {"key" "value"}))))

(defn test-extract-json-ok-array []
  (let [r (hyjinx.lib.extract_json "result: [1, 2, 3]")]
    (assert (get r "ok"))
    (assert (= (unwrap r) [1 2 3]))))

(defn test-extract-json-err-no-json []
  (let [r (hyjinx.lib.extract_json "plain text with no json content")]
    (assert (not (get r "ok")))
    (assert (= (get r "error" "type") "no-json-found"))))

(defn test-extract-json-err-malformed []
  (let [r (hyjinx.lib.extract_json "result: {broken json here")]
    (assert (not (get r "ok")))))

(defn test-extract-json-empty-string []
  (let [r (hyjinx.lib.extract_json "")]
    (assert (not (get r "ok")))
    (assert (= (get r "error" "type") "no-json-found"))))

(defn test-extract-json-nested-object []
  (let [text "data: {\"a\": {\"b\": [1, 2]}, \"c\": true}"
        r (hyjinx.lib.extract_json text)]
    (assert (get r "ok"))
    (let [data (unwrap r)]
      (assert (= (get (get data "a") "b") [1 2])))))

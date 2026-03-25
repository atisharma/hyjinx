"
Tests for hyjinx.result — the Result type for explicit error handling.
"

(import hyjinx.result [ok err as-result unwrap unwrap-or map-ok map-err collect-results])

(require hyrule [-> ->>])
(require hyjinx.result [result-> result->> let-result match-result try-result])


;; ── Constructors ─────────────────────────────────────────────────────────────

(defn test-ok-shape []
  (let [r (ok 42)]
    (assert (= r {"ok" True "value" 42}))))

(defn test-ok-none []
  (let [r (ok None)]
    (assert (is (get r "ok") True))
    (assert (is (get r "value") None))))

(defn test-err-shape []
  (let [r (err "validation-error" "must be positive" {"field" "x"})]
    (assert (is (get r "ok") False))
    (assert (= (get r "error" "type") "validation-error"))
    (assert (= (get r "error" "message") "must be positive"))
    (assert (= (get r "error" "context") {"field" "x"}))))

(defn test-err-default-context []
  (let [r (err "some-error" "message")]
    (assert (= (get r "error" "context") {}))))

(defn test-ok-wraps-any-value []
  (for [v [0 "" [] {} False 3.14]]
    (assert (= (get (ok v) "value") v))))


;; ── as-result ───────────────────────────────────────────────────────────────

(defn test-as-result-passthrough []
  (let [r (ok 5)]
    (assert (is (as-result r) r))))

(defn test-as-result-wraps-plain []
  (let [r (as-result 99)]
    (assert (is (get r "ok") True))
    (assert (= (get r "value") 99))))


;; ── Utilities ────────────────────────────────────────────────────────────────

(defn test-unwrap-ok []
  (assert (= (unwrap (ok "hello")) "hello")))

(defn test-unwrap-err-raises []
  (try
    (unwrap (err "e" "msg"))
    (except [e AssertionError]
      (assert True))))

(defn test-unwrap-or-ok []
  (assert (= (unwrap-or (ok 10) 99) 10)))

(defn test-unwrap-or-err []
  (assert (= (unwrap-or (err "e" "msg") 99) 99)))

(defn test-map-ok-transforms-value []
  (let [r (map-ok (fn [x] (* x 2)) (ok 5))]
    (assert (is (get r "ok") True))
    (assert (= (get r "value") 10))))

(defn test-map-ok-passthrough-err []
  (let [e (err "e" "msg")]
    (assert (is (map-ok (fn [x] (* x 2)) e) e))))

(defn test-map-err-transforms-error []
  (let [e (err "old-type" "old message")
        r (map-err (fn [e] (dict e :type "new-type")) e)]
    (assert (is (get r "ok") False))
    (assert (= (get r "error" "type") "new-type"))))

(defn test-map-err-passthrough-ok []
  (let [o (ok 42)]
    (assert (is (map-err (fn [e] e) o) o))))

(defn test-collect-results-all-ok []
  (let [results [(ok 1) (ok 2) (ok 3)]
        r (collect-results results)]
    (assert (is (get r "ok") True))
    (assert (= (get r "value") [1 2 3]))))

(defn test-collect-results-first-err []
  (let [results [(ok 1) (err "e" "boom") (ok 3)]
        r (collect-results results)]
    (assert (is (get r "ok") False))
    (assert (= (get r "error" "message") "boom"))))

(defn test-collect-results-empty []
  (let [r (collect-results [])]
    (assert (is (get r "ok") True))
    (assert (= (get r "value") []))))


;; ── Threading macros ─────────────────────────────────────────────────────────

(defn test-result-arrow-short-circuits-on-err []
  (defn bad-step [x] (err "fail" "exploded"))
  (defn double [x] (* x 100))
  (let [result (result-> (ok 1) bad-step double)]
    (assert (is (get result "ok") False))
    (assert (= (get result "error" "message") "exploded"))))

(defn test-result-arrow-propagates-value []
  (let [result (result-> (ok 3) (+ 1) (* 2))]
    (assert (is (get result "ok") True))
    (assert (= (get result "value") 8))))

(defn test-result-arrow-wraps-plain-returns []
  (defn triple [x] (* x 3))
  (let [result (result-> 5 triple)]
    (assert (is (get result "ok") True))
    (assert (= (get result "value") 15))))


;; ── Binding macros ───────────────────────────────────────────────────────────

(defn test-let-result-binds-value []
  (let [result (let-result [x (ok 42)]
                 (ok (* x 2)))]
    (assert (is (get result "ok") True))
    (assert (= (get result "value") 84))))

(defn test-let-result-propagates-err []
  (let [result (let-result [x (err "fail" "no value")]
                 (ok (* x 2)))]
    (assert (is (get result "ok") False))
    (assert (= (get result "error" "message") "no value"))))

(defn test-match-result-ok-branch []
  (let [result (match-result (ok 10)
                  v (* v 3)
                  e -1)]
    (assert (= result 30))))

(defn test-match-result-err-branch []
  (let [result (match-result (err "e" "bad input")
                  v (* v 3)
                  e (get e "message"))]
    (assert (= result "bad input"))))


;; ── Exception bridging ───────────────────────────────────────────────────────

(defn test-try-result-success []
  (let [result (try-result (int "42"))]
    (assert (is (get result "ok") True))
    (assert (= (get result "value") 42))))

(defn test-try-result-exception []
  (let [result (try-result (int "not-a-number"))]
    (assert (is (get result "ok") False))
    (assert (= (get result "error" "type") "ValueError"))))

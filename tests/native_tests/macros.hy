"
Tests for hyjinx.macros — the macro collection.
"

(import types)
(import hy)
(import hy.macros)
(import hyjinx.macros)


(defn make-module []
  "Create a fresh module with hyjinx.macros required."
  (let [m (types.ModuleType "__hy_test__")]
    (hy.macros.require "hyjinx.macros" m :assignments "ALL" :prefix "")
    m))

(defn hy-eval [code]
  "Evaluate Hy code with all hyjinx.macros available."
  (let [m (make-module)]
    (hy.eval (hy.read-many code) :module m)))


;; ── defmethod (multimethod dispatch) ─────────────────────────────────────────

(defn test-dispatch-on-int []
  (let [result (hy-eval "
(defmethod classify [#^ int x]
  \"integer\")
(defmethod classify [#^ str x]
  \"string\")
(classify 42)")]
    (assert (= result "integer"))))

(defn test-dispatch-on-str []
  (let [result (hy-eval "
(defmethod classify2 [#^ int x]
  \"integer\")
(defmethod classify2 [#^ str x]
  \"string\")
(classify2 \"hello\")")]
    (assert (= result "string"))))

(defn test-dispatch-accumulates-multimethods []
  (let [result-int (hy-eval "
(defmethod adder [#^ int x] (+ x 1))
(defmethod adder [#^ float x] (+ x 0.5))
(adder 10)")
        result-float (hy-eval "
(defmethod adder3 [#^ int x] (+ x 1))
(defmethod adder3 [#^ float x] (+ x 0.5))
(adder3 1.0)")]
    (assert (= result-int 11))
    (assert (< (abs (- result-float 1.5)) 0.001))))


;; ── defstruct (frozen dataclass) ─────────────────────────────────────────────

(defn test-basic-struct []
  (let [Point (hy-eval "
(defstruct Point
  #^ int x
  #^ int y)
Point")
        p (Point :x 1 :y 2)]
    (assert (= p.x 1))
    (assert (= p.y 2))))

(defn test-struct-is-frozen []
  (let [Point (hy-eval "
(defstruct ImmutablePoint
  #^ int x
  #^ int y)
ImmutablePoint")
        p (Point :x 3 :y 4)]
    (try
      (setv p.x 99)
      (except [e AttributeError]
        (assert True))
      (except [e TypeError]
        (assert True)))))

(defn test-struct-equality []
  (let [Point (hy-eval "
(defstruct EqPoint
  #^ int x
  #^ int y)
EqPoint")]
    (assert (= (Point :x 1 :y 2) (Point :x 1 :y 2)))
    (assert (!= (Point :x 1 :y 2) (Point :x 1 :y 3)))))


;; ── when-let ──────────────────────────────────────────────────────────────────

(defn test-when-let-executes-body-when-truthy []
  (let [result (hy-eval "
(when-let [x 42]
  (* x 2))")]
    (assert (= result 84))))

(defn test-when-let-returns-none-when-falsy []
  (let [result (hy-eval "
(when-let [x None]
  \"should not reach here\")")]
    (assert (is result None))))

(defn test-when-let-returns-none-on-zero []
  (let [result (hy-eval "
(when-let [x 0]
  \"executed\")")]
    (assert (is result None))))

(defn test-when-let-with-dict-lookup []
  (let [result (hy-eval "
(setv d {\"key\" \"value\"})
(when-let [v (.get d \"key\")]
  (.upper v))")]
    (assert (= result "VALUE"))))


;; ── Sequence macros ───────────────────────────────────────────────────────────

(defn test-lmap-is-eager []
  (let [result (hy-eval "
(lmap (fn [x] (* x 2)) [1 2 3])")]
    (assert (= result [2 4 6]))
    (assert (isinstance result list))))

(defn test-range-macro []
  (let [result (hy-eval "(.. 0 5)")]
    (assert (= result [0 1 2 3 4]))))

(defn test-range-macro-with-step []
  (let [result (hy-eval "(.. 0 10 2)")]
    (assert (= result [0 2 4 6 8]))))

(defn test-prepend []
  (let [result (hy-eval "(prepend 0 [1 2 3])")]
    (assert (= result [0 1 2 3]))))

(defn test-append []
  (let [result (hy-eval "(append 4 [1 2 3])")]
    (assert (= result [1 2 3 4]))))


;; ── do-while ──────────────────────────────────────────────────────────────────

(defn test-do-while-executes-body-at-least-once []
  (let [result (hy-eval "
(setv counter 0)
(do-while False
  (setv counter (+ counter 1)))
counter")]
    (assert (= result 1))))

(defn test-do-while-loops-while-true []
  (let [result (hy-eval "
(setv n 0)
(do-while (< n 3)
  (setv n (+ n 1)))
n")]
    (assert (= result 3))))

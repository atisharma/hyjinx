"
result.hy — Idiomatic Result type for Hy

A lightweight, data-oriented error handling system. Results are plain dicts,
observable and serialisable. The design follows Hickey-style data flow and
Duffy's error model: expected failures return Results, bugs raise/assert.

## Quick reference

Constructors:   ok, err
Threading:      result->, result->>
Binding:        let-result, match-result
Bridging:       try-result, raise-if-err
Utilities:      unwrap, unwrap-or, map-ok, map-err, as-result

## Result shape

  {\"ok\" True  \"value\" <any>}                          ; success
  {\"ok\" False \"error\" {\"type\" str \"message\" str    ; failure
                          \"context\" dict}}

## Usage patterns

Direct dict access:
  (if (:ok result)
    (do-something (:value result))
    (handle-error (:error result)))

Pattern match style:
  (match result
    {\"ok\" True \"value\" v} (process v)
    {\"ok\" False \"error\" e} (fail e))

## Constraint: no backslash in Hy f-strings
Dict access inside f-strings requires a let binding:
  (let [msg (get e \"message\")] f\"Failed: {msg}\")

## Import

  (require hyjinx.result [result-> result->> let-result match-result try-result])
  (import hyjinx.result [ok err as-result unwrap unwrap-or map-ok map-err raise-if-err])
"

(require hyrule [-> ->>])


;; ============================================================
;; Core data constructors
;; ============================================================

(defn ok [value]
  "Construct a success Result wrapping value."
  {"ok" True "value" value})

(defn err [error-type message [context {}]]
  "Construct a failure Result.
  error-type: short string identifier, e.g. 'validation-error', 'network-error'.
  message:    human-readable description.
  context:    optional dict of structured context (URLs, keys, values, etc.)."
  {"ok" False "error" {"type" error-type "message" message "context" context}})


;; ============================================================
;; Helpers (internal)
;; ============================================================

(defn as-result [x]
  "Coerce x to a Result. If already a Result, return unchanged; else wrap in ok.
  Used by result-> to allow plain-value steps in pipelines.
  
  A Result is any dict with an \"ok\" key."
  (if (and (isinstance x dict) (in "ok" x))
    x
    (ok x)))


;; ============================================================
;; Threading macros
;; ============================================================

(defmacro result-> [expr #* forms]
  "Thread expr through forms, short-circuiting on the first err Result.
  Each form receives the unwrapped value as its FIRST argument (mirrors ->).
  Plain return values from any step are wrapped in ok automatically.

  Example:
    (result-> raw-input
              (parse-int)             ; str -> Result[int]
              (validate-range 0 100)  ; int -> Result[int] (value as 1st arg)
              (* 2))                  ; int -> int (wrapped in ok automatically)
  "
  (if (= (len forms) 0)
    `(as-result ~expr)
    (let [r (hy.gensym "r")]
      `(let [~r (as-result ~expr)]
         (if (:ok ~r)
           (result-> (-> (:value ~r) ~(get forms 0)) ~@(cut forms 1 None))
           ~r)))))

(defmacro result->> [expr #* forms]
  "Thread expr through forms, short-circuiting on the first err Result.
  Each form receives the unwrapped value as its LAST argument (mirrors ->>).
  Plain return values are wrapped in ok automatically.

  Example:
    (result->> raw-string
               (.strip)
               (.lower)
               (re.sub r\"\\s+\" \" \" it))  ; value as last arg
  "
  (if (= (len forms) 0)
    `(as-result ~expr)
    (let [r (hy.gensym "r")]
      `(let [~r (as-result ~expr)]
         (if (:ok ~r)
           (result->> (->> (:value ~r) ~(get forms 0)) ~@(cut forms 1 None))
           ~r)))))


;; ============================================================
;; Binding macros
;; ============================================================

(defmacro let-result [bindings #* body]
  "Bind the value of a successful Result to a name; propagate err unchanged.
  Mirrors when-let but for Results — the body must return a Result.

  Usage:
    (defn process [s]
      (let-result [n (parse-int s)]       ; n is bound to the unwrapped int
        (let-result [v (validate n)]
          (ok (* v 2)))))                 ; final body returns a Result

  On err input, returns the err immediately without evaluating body."
  (let [sym (get bindings 0)
        expr (get bindings 1)
        r (hy.gensym "r")]
    `(let [~r ~expr]
       (if (:ok ~r)
         (let [~sym (:value ~r)] ~@body)
         ~r))))

(defmacro match-result [result ok-sym ok-body err-sym err-body]
  "Pattern match on a Result, binding ok-sym or err-sym.

  Usage:
    (match-result (fetch-user id)
      user  (render user)              ; ok branch: user = the value
      error (let [msg (get error \"message\")]
              (render-error msg)))     ; err branch: error = the error dict

  Note: dict access in f-strings requires a let binding (no backslash in f-strings)."
  (let [r (hy.gensym "r")]
    `(let [~r ~result]
       (if (:ok ~r)
         (let [~ok-sym (:value ~r)] ~ok-body)
         (let [~err-sym (:error ~r)] ~err-body)))))


;; ============================================================
;; Exception bridging
;; ============================================================

(defmacro try-result [#* body]
  "Evaluate body and wrap the outcome in a Result.
  Returns ok on success, err on any Exception.

  ONLY use this at known fallible boundaries: network calls, parsing,
  file I/O, external APIs. Do not use as a general-purpose try/except
  — that would swallow bugs silently.

  The error type is the exception class name (e.g. 'ValueError').
  The original exception is stored in context under 'exception'."
  `(try
     (ok (do ~@body))
     (except [e Exception]
       (err (. (type e) __name__) (str e) {"exception" e}))))

(defn raise-if-err [result]
  "Convert an err Result back into an exception.
  Use at boundaries between Result-world and exception-world,
  e.g. at the top of a request handler that must raise on failure.
  Do NOT use to bridge expected failures back into bugs."
  (when (not (:ok result))
    (let [e (:error result)
          msg (get e "message")
          t (get e "type")]
      (raise (RuntimeError f"{t}: {msg}"))))
  result)


;; ============================================================
;; Utilities
;; ============================================================

(defn unwrap [result]
  "Extract the value from an ok Result.
  PANICS (AssertionError) if result is err.

  Only call this when you have already checked (:ok result), or in tests,
  or at a point where an err is genuinely impossible (document why).
  Prefer match-result or let-result in production code."
  (assert (:ok result) "unwrap called on err result")
  (:value result))

(defn unwrap-or [result default]
  "Extract the value from an ok Result, or return default on err.
  Use when a fallback value is appropriate and err is non-exceptional."
  (if (:ok result) (:value result) default))

(defn map-ok [f result]
  "Apply f to the value inside an ok Result; pass err through unchanged.
  f may return either a plain value (wrapped in ok) or a Result.

  Use for transformations in the middle of a pipeline when you don't
  want the full threading macro overhead."
  (if (:ok result)
    (as-result (f (:value result)))
    result))

(defn map-err [f result]
  "Apply f to the error dict inside an err Result; pass ok through unchanged.
  f should return a new error dict (for enrichment/translation).

  Use to add context at a layer boundary, e.g.:
    (map-err (fn [e] (| e {\"layer\" \"db\"})) db-result)"
  (if (:ok result)
    result
    {"ok" False "error" (f (:error result))}))

(defn collect-results [results]
  "Given a sequence of Results, return:
    ok([values]) if all succeeded
    err of the FIRST failure encountered

  Fail-fast: stops at the first err. Use when all steps must succeed."
  (let [values []]
    (for [r results]
      (if (:ok r)
        (.append values (:value r))
        (return r)))
    (ok values)))

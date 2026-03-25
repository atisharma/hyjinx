"
Tests for hyjinx.actors — the asyncio actor framework.
"

(import asyncio)
(import hyjinx.actors [Actor])


;; ── Concrete actor implementations for testing ────────────────────────────────

(defclass EchoActor [Actor]
  (defn :async recv [self message]
    message))

(defclass UpperActor [Actor]
  (defn :async recv [self message]
    (.upper message)))

(defclass AddingActor [Actor]
  (defn :async recv [self message]
    (+ (get message 0) (get message 1))))

(defclass BoomActor [Actor]
  (defn :async recv [self message]
    (raise (ValueError f"boom: {message}"))))

(defclass SlowActor [Actor]
  (defn :async recv [self message]
    (await (asyncio.sleep 0.01))
    f"processed: {message}"))


;; ── Lifecycle ─────────────────────────────────────────────────────────────────

(defn test-start-sets-started-flag []
  (asyncio.run
    ((fn :async []
       (let [a (EchoActor)]
         (a.start)
         (assert (is a._started True))
         (await (a.stop)))))))

(defn test-double-start-raises []
  (asyncio.run
    ((fn :async []
       (let [a (EchoActor)]
         (a.start)
         (try
           (a.start)
           (except [e RuntimeError]
             (assert (in "already started" (str e)))))
         (await (a.stop)))))))

(defn test-stop-clears-started-flag []
  (asyncio.run
    ((fn :async []
       (let [a (EchoActor)]
         (a.start)
         (await (a.stop))
         (assert (is a._started False)))))))

(defn test-send-before-start-raises []
  (asyncio.run
    ((fn :async []
       (let [a (EchoActor)]
         (try
           (await (a.send "hello"))
           (except [e RuntimeError]
             (assert (in "not started" (str e))))))))))

(defn test-ask-before-start-raises []
  (asyncio.run
    ((fn :async []
       (let [a (EchoActor)]
         (try
           (await (a.ask "hello"))
           (except [e RuntimeError]
             (assert (in "not started" (str e))))))))))

(defn test-context-manager []
  (defn :async run-test []
    (with [:async actor (EchoActor)]
      (await (actor.ask "hello"))))
  (let [result (asyncio.run (run-test))]
    (assert (= result "hello"))))

(defn test-status-stopped []
  (let [a (EchoActor)]
    (assert (= (get a.status "state") "stopped"))))

(defn test-status-running []
  (defn :async run-test []
    (let [a (EchoActor)]
      (a.start)
      (let [s a.status]
        (await (a.stop))
        s)))
  (let [status (asyncio.run (run-test))]
    (assert (= (get status "state") "running"))))


;; ── send (fire-and-forget) ────────────────────────────────────────────────────

(defn test-send-does-not-raise []
  (defn :async run-test []
    (with [:async actor (EchoActor)]
      (await (actor.send "test message"))))
  (asyncio.run (run-test)))

(defn test-send-with-exception-does-not-propagate []
  (defn :async run-test []
    (with [:async actor (BoomActor)]
      (await (actor.send "trigger"))
      (await (asyncio.sleep 0.05))))
  (asyncio.run (run-test)))


;; ── ask (request-reply) ───────────────────────────────────────────────────────

(defn test-ask-returns-value []
  (defn :async run-test []
    (with [:async actor (EchoActor)]
      (await (actor.ask "ping"))))
  (let [result (asyncio.run (run-test))]
    (assert (= result "ping"))))

(defn test-ask-upper []
  (defn :async run-test []
    (with [:async actor (UpperActor)]
      (await (actor.ask "hello world"))))
  (let [result (asyncio.run (run-test))]
    (assert (= result "HELLO WORLD"))))

(defn test-ask-with-tuple []
  (defn :async run-test []
    (with [:async actor (AddingActor)]
      (await (actor.ask #(3 7)))))
  (let [result (asyncio.run (run-test))]
    (assert (= result 10))))

(defn test-ask-propagates-exception []
  (defn :async run-test []
    (with [:async actor (BoomActor)]
      (await (actor.ask "oops"))))
  (try
    (asyncio.run (run-test))
    (except [e ValueError]
      (assert (in "boom: oops" (str e))))))

(defn test-ask-multiple-sequential []
  (defn :async run-test []
    (with [:async actor (AddingActor)]
      (lfor i (range 5)
        (await (actor.ask #(i i))))))
  (let [results (asyncio.run (run-test))]
    (assert (= results [0 2 4 6 8]))))

(defn test-ask-slow-actor []
  (defn :async run-test []
    (with [:async actor (SlowActor)]
      (await (actor.ask "data"))))
  (let [result (asyncio.run (run-test))]
    (assert (= result "processed: data"))))


;; ── defactor macro ────────────────────────────────────────────────────────────

(defn test-defactor-creates-actor []
  (import hy)
  (let [Greeter (hy.eval (hy.read_many "
(import hyjinx.actors [Actor])
(defmacro defactor [a args #* body]
  `(do
     (import hyjinx.actors [Actor])
     (defclass ~a [Actor]
       (defn :async recv [self ~@args]
         ~@body))))
(defactor Greeter [#^ str name]
  (.upper name))
Greeter"))]
    (defn :async run-test []
      (with [:async actor (Greeter)]
        (await (actor.ask "alice"))))
    (let [result (asyncio.run (run-test))]
      (assert (= result "ALICE")))))

(defn test-defactor-via-import []
  (import hy)
  (let [Doubler (hy.eval (hy.read_many "
(import hyjinx.actors [Actor])
(defmacro defactor [a args #* body]
  `(do
     (import hyjinx.actors [Actor])
     (defclass ~a [Actor]
       (defn :async recv [self ~@args]
         ~@body))))
(defactor Doubler [#^ int n]
  (* n 2))
Doubler"))]
    (defn :async run-test []
      (with [:async actor (Doubler)]
        (await (actor.ask 21))))
    (let [result (asyncio.run (run-test))]
      (assert (= result 42)))))


;; ── Queue/status metrics ──────────────────────────────────────────────────────

(defn test-status-has-required-keys []
  (defn :async run-test []
    (with [:async actor (EchoActor)]
      actor.status))
  (let [status (asyncio.run (run-test))]
    (assert (in "state" status))
    (assert (in "metrics" status))
    (assert (in "health" status))
    (assert (in "running_tasks" (get status "metrics")))
    (assert (in "queue_size" (get status "metrics")))))

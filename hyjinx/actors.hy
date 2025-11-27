"
Actor model implementation using asyncio for concurrency.

This module provides a lightweight, async actor framework where each actor receives 
messages sequentially from an asyncio.Queue, ensuring async-safe, isolated execution 
without explicit locks. It is not thread-safe.

Key features:
- `Actor`: Base class requiring `recv()` implementation for message handling.
- `send()`: Fire-and-forget messaging; exceptions logged, no reply expected.
- `ask()`: Request-reply pattern; caller awaits result or propagates exception.
- Lifecycle management via `start()`/`stop()` and async context manager support.
- Automatic task tracking and graceful shutdown.

Use `defactor` to quickly define stateless actor classes. Avoid `await` in `recv` 
if strict message ordering is required.

Example:
    (defactor Greeter [#^str name] (print f\"Hello, {name}!\"))
    (with [:async actor (Greeter)] (await (.send actor \"Alice\")))
"


(require hyjinx [when-let])

(import abc [ABC abstractmethod])
(import asyncio)
(import typing [Generic TypeVar])
(import logging)


(setv T (TypeVar "T"))


(defclass Actor [(get Generic T) ABC]
  "Base class for actors."

  (defn __init__ [self * [maxsize 1024]]
    (setv self._queue (asyncio.Queue maxsize))
    (setv self._running-tasks (set))
    (setv self._tasks-lock (asyncio.Lock))
    (setv self._started False)
    (setv self._stop-event (asyncio.Event))
    (setv self._bg-task None)
    (setv self._tick 0.1)
    (setv self._logger (logging.getLogger (. (type self) __name__))))

  (defn start [self]
    "Start the actor, initializing pseudo background event loop."
    (when self._started
      (raise (RuntimeError "Actor is already started")))
    
    (try
      (setv self._started True)
      (.clear self._stop-event)
      (setv self._bg-task (asyncio.create-task (self._background-event-loop)))
      (except [e [Exception]]
        (setv self._started False)
        (setv self._bg-task None)
        (raise e)))
    self)

  (defn :async _add-task [self task]
    "Thread-safe task addition."
    (with [:async self._tasks-lock]
      (.add self._running-tasks task)))

  (defn :async _background-event-loop [self]
    "The internal event loop.
    Process while not stopped, then drain the queue."
    (while (not (self._stop-event.is-set))
      (try
        (let [;; Destructure the message and the reply channel
              #(message reply-future) (await (asyncio.wait-for (.get self._queue) :timeout self._tick))
              handler-task (asyncio.create_task (self.recv message))]
          ;; Attach the future to the task object itself for retrieval in the callback
          (setv handler-task._reply_future reply-future)
          (await (self._add-task handler-task))
          (.add-done-callback handler-task (fn [t] (self._handle-task-completion t))))
        (except [asyncio.TimeoutError]
          (continue))))

    ;; Process remaining messages on shutdown so all tasks are awaited
    (setv remaining-tasks [])
    (while True
      (try
        (let [;; Destructure the message payload and the reply channel
              #(message reply-future) (.get-nowait self._queue)
              handler-task (asyncio.create_task (self.recv message))]
          ;; Attach the future to the task object itself for retrieval in the callback
          (setv handler-task._reply_future reply-future)
          (await (self._add-task handler-task))
          (.add-done-callback handler-task (fn [t] (self._handle-task-completion t)))
          (.append remaining-tasks handler-task))
        (except [asyncio.QueueEmpty]
          (break))))

    (when remaining-tasks
      (await (asyncio.wait remaining-tasks))))

  (defn _handle-task-completion [self task]
    "Resolve the reply future based on task outcome."
    (try
      (let [reply-future (getattr task "_reply_future" None) ; note Hy mangling rules
            exception (try
                        (task.exception)
                        (except [asyncio.CancelledError]
                          :cancelled))]
      
        (if reply-future
          ;; ask: propagate result or exception to the waiting caller
          (cond
            (= exception :cancelled)
            (when (not (reply-future.done))
              (.cancel reply-future))
            
            exception
            (.set_exception reply-future exception)
            
            True
            (.set_result reply-future (task.result)))
        
          ;; send: log exceptions but don't propagate
          (when (and exception (!= exception :cancelled))
            (self._logger.debug f"Handler raised exception in actor task {task}" :exc-info True))))
            ;(import traceback sys)
            ;(traceback.print-exception (type exception) exception (. exception __traceback__) :file sys.stderr))))
    
      (except [asyncio.InvalidStateError]) ; Future already resolved/cancelled
      (except [e [Exception]]
        ;; Catch any unexpected errors in the callback itself
        (self._logger.error f"Unexpected exception in task completion handler: {e}"))
      (finally
        (self._remove-task task))))

  (defn _remove-task [self task]
    "Remove a task from tracking set.
    Fail silently if the task is not present."
    (try
      (.discard self._running-tasks task)
      (except [Exception])))

  (defn :async stop [self]
    "Shut down the actor.
    Wait for all running tasks to complete."
    (self._verify-started)
    
    ;; Signal stop, reject new tasks
    (setv self._started False)
    (self._stop-event.set)
    
    ;; Wait for background task
    (when self._bg-task
      (await self._bg-task))
    
    ;; After _bg-task completes, all drain tasks are already started
    ;; take a locked snapshot before await
    (with [:async self._tasks-lock]
      (setv remaining (set self._running-tasks)))

    ;; Just wait for whatever may have been started during the bg loop drain
    (when remaining
      (await (asyncio.wait remaining))))

  (defn _verify-started [self]
    (when (not self._started)
      (raise (RuntimeError "Actor is not started"))))

  (defn [property] status [self]
    "The health, status and metrics of the actor."
    {"state" (cond
               (not self._started) "stopped"
               (.is-set self._stop-event) "stopping"
               :else "running")
     "metrics" {"running_tasks" (len self._running-tasks)
                "queue_size" (self._queue.qsize)
                "queue_utilization" (/ (self._queue.qsize) self._queue._maxsize)
                "queue_empty" (self._queue.empty)}
     "health" {"queue_full" (self._queue.full)
               "background_task_alive" (and self._bg-task
                                            (not (self._bg-task.done)))}}) 

  (defn :async __aenter__ [self]
    (self.start))

  (defn :async __aexit__ [self exc-type exc-val exc-tb]
    (await (self.stop))
    False)

  (defn :async [abstractmethod] recv [self message]
    "Process a message sent to the actor.
    
    This method must be implemented by subclasses. It is called for each
    message received via `send` or `ask`. Return the response value for `ask`
    requests; raise exceptions to signal errors. For `send`, returned values
    are ignored.

    Note: If this method uses `await`, message processing order is not
    guaranteed. For strictly sequential handling, avoid async operations or use
    external synchronization.")

  (defn :async #^ None send [self message]
    "Send a message to the actor asynchronously (fire-and-forget).
    
    The caller does not wait for or receive any response. Exceptions in the
    message handler are logged but not propagated. Message processing order is
    preserved only if `recv` does not yield control (i.e. no `await` calls)."
    (self._verify-started)
    (await (self._queue.put #(message None))))

  (defn :async #^ T ask [self message]
    "Send a message and asynchronously wait for a reply.
    
    Implements request-reply: returns the handler's return value,
    or raises any exception it raised. Safe to use with `await`.
    Message processing order is not preserved if `recv` contains `await`."
    (self._verify-started)
    (setv reply-future (asyncio.Future))
    (await (self._queue.put #(message reply-future)))
    (await reply-future)))

(defmacro defactor [a args #* body]
  "Sugar to define a stateless `Actor` subclass.
  
  This macro creates an Actor subclass where the message is destructured and
  passed directly to the handler. It should not be used to maintain
  per-instance mutable state, otherwise race conditions will occur.

  Example usage:

    (defactor Printer [#^ str msg]
      (print msg))

    Then, in the running event loop,

    (setv p (Printer))
    (.start p)
    (await (.send p \"a string\")
    (await (.stop p))
  
    Or,

    (with [:async p (Printer)]
      (await (.send p \"another string\")
  "
  `(do
     (import hyjinx.actors [Actor])
     (defclass ~a [Actor]
       (defn :async recv [self ~@args]
         ~@body))))
  

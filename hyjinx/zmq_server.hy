"
Lazy Pirate server pattern.
Starts the socket on import.
"

(require hyrule.argmove [-> ->>])

(import asyncio)
(import json)
(import zmq)
(import zmq.asyncio)

(import logging)
(import hyjinx.crypto)
(import hyjinx.lib [repeat config hash-id])
(import hyjinx.wire [wrap unwrap zerror])


(setv SendError (Exception "Message (reply) send failed.")
      ServerError (Exception))

(setv N_CONCURRENT_CLIENTS 1000
      BACKGROUND_TICK 1)

(setv _conf (config "server.toml")
      context (zmq.asyncio.Context)
      socket (.socket context zmq.ROUTER))

(defn start-router-socket [address]
  (setv socket (.socket context zmq.ROUTER))
  ; see https://stackoverflow.com/questions/26915347/zeromq-reset-req-rep-socket-state
  ; server will tick every 2s
  (.setsockopt socket zmq.RCVTIMEO 2000)
  (.setsockopt socket zmq.SNDTIMEO 1000)
  (.setsockopt socket zmq.LINGER 2000)
  (.bind socket address)
  socket)

(setv frontend (start-router-socket (:listen _conf)))


(defn :async send [zmsg]
  "Send a reply to a client."
  (try
    (await (.send-multipart frontend zmsg))
    (except [zmq.EAGAIN]
      (raise SendError))))
  
(defn verify [pub-key msg]
  "Check the message against its signature."
  (let [payload (:payload msg {})
        signature (:signature msg "")
        client-time (:sender-time msg Inf)
        expected-hash (hash-id (+ client-time (json.dumps payload)))]
    (crypto.verify pub-key signature expected-hash)))

(defn :async server-loop [f [id 0]]
  "Wait for a message, then call (f msg).
For example, f may verify signature, then call a method, then send the reply (verified rpc)."
  (while True
    (try
      (await (f (await (.recv-multipart frontend))))
      (except [zmq.Again])
      (except [KeyboardInterrupt]
        (logging.info f"Interrupted, closing {id}")
        (break))
      (except [err [Exception]]
        (logging.error f"server-loop {id} error:" :exception err)
        (raise (ServerError (repr err)))))))

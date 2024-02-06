"
Lazy Pirate client pattern.
Starts the socket on import.
"

(import time [sleep])
(import random [randint])
(import json)
(import zmq)

(import wire [wrap unwrap zerror])
(import lib [config])


(setv context (zmq.Context))

(setv conf (config "client.toml")
      REQUEST_TIMEOUT_S 20
      context (zmq.Context))

(defn start-socket []
  (setv socket (.socket context zmq.REQ))
  ; see https://stackoverflow.com/questions/26915347/zeromq-reset-req-rep-socket-state
  (.setsockopt socket zmq.RCVTIMEO (* REQUEST_TIMEOUT_S 1000))
  (.setsockopt socket zmq.REQ_CORRELATE 1)
  (.setsockopt socket zmq.REQ_RELAXED 1)
  (.setsockopt socket zmq.LINGER 1000)
  (.connect socket f"tcp://{(:server conf)}:{(:port conf)}")
  socket)

(setv socket (start-socket))

(defn rpc [method #* args #** kwargs]
  "Call a method on the server. Return None for timeout."
  (try
    (.send socket (wrap {"method" method "args" args "kwargs" kwargs}))
    (:payload (unwrap (.recv socket)))
    (except [zmq.Again]
      (zerror "The request timed out."))))

;; example call
(defn motd [#* args #** kwargs]
  (rpc {"method" "motd"
        "args" args
        "kwargs" kwargs}))

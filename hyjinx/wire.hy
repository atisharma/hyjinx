(require hyrule [->])

(import zmq)
(import json)
(import time [time])
(import uuid [uuid1])
(import zstandard [compress decompress])

(import crypto)
(import lib [config hash-id])


(setv sender-id (. (uuid1) hex))

(setv HYJINX_PROTOCOL_VERSION "0.0.2")

(setv keys (crypto.keys (config "passphrase"))
      priv-key (:private keys)
      pub-key (:public-pem keys))

(defn wrap [payload]
  "Format and wrap message."
  (let [t (time)
        payload-hash (hash-id (+ (str t) (str payload)))]
    (-> (json.dumps {"payload" payload
                     "proto_version" HYJINX_PROTOCOL_VERSION
                     "zmq_version" zmq.__version__
                     "sender_id" sender-id
                     "sender_time" t
                     "public_key" pub-key
                     "signature" (crypto.sign priv-key payload-hash)})
        (.encode)
        (compress))))

(defn unwrap [zmsg]
  "Having recieved via recv_multipart."
  (try
    (-> zmsg
        (.decode zmsg)
        (decompress)
        (json.loads)) 
    (except [json.JSONDecodeError]
      (zerror "Failed to decode message JSON."))))

(defn zerror [text]
  {"error" text})

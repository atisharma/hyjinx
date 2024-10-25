(require hyrule [->])

(import zmq)
(import json)
(import time [time])
(import uuid [uuid1])
(import zstandard [compress decompress])

(import hyjinx [crypto])
(import hyjinx.lib [config hash-id])


(setv sender-id (. (uuid1) hex))

(setv HYJINX_PROTOCOL_VERSION "0.0.3")

(defn keys [config-file]
  "Return dict with public and private keys."
  (let [keys (crypto.keys (:passphrase config config-file))]
    {"priv-key" (:private keys)
     "pub-key" (:public-pem keys)}))

(defn wrap [payload * pub-key priv-key]
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
      (zerror :code "JSON" :message "Failed to decode message JSON."))))

(defn zerror [message]
  {"error" {"code" code "message" message}})

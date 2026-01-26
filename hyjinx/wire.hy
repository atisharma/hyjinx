(require hyrule [->])
(import hyrule [assoc])

(import zmq)
(import msgpack [packb unpackb UnpackException])
(import time [time])
(import uuid [uuid1])
(import zstandard [compress decompress])

(import hyjinx [crypto])
(import hyjinx.lib [config hash-id])


(setv sender-id (. (uuid1) hex))
(setv HYJINX_PROTOCOL_VERSION "0.2.0")
(setv rpcs {})


(defn keys [config-file #^ str salt]
  "Return dict with public and private keys for use with wrap.
  These are derived from the passphrase defined in `config-file`.
  The salt should be a random string."
  (let [passphrase (:passphrase (config config-file))
        keys (crypto.keys passphrase salt)]
    {"priv_key" (:private keys)
     "pub_key" (:public-pem keys)}))

(defn wrap [payload * [pub-key None] [priv-key None]]
  "Format and wrap message."
  (let [t (time)
        payload-hash (hash-id (+ (str t) (str payload)))]
    (-> {"payload" payload
         "proto_version" HYJINX_PROTOCOL_VERSION
         "zmq_version" zmq.__version__
         "sender_id" sender-id
         "sender_time" t
         "public_key" pub-key
         "signature" (when priv-key
                       (crypto.sign priv-key payload-hash))}
        (packb)
        (compress))))

(defn unwrap [zmsg]
  "Having recieved via recv_multipart."
  (try
    (-> zmsg
      (decompress)
      (unpackb))
    (except [UnpackException]
      (zerror :code "msgpack" :message "Failed to unpack message."))))

;; TODO consider doing as a sumtype
(defn zerror [message]
  {"error" {"code" code "message" message}})

(defn rpc [f]
  "Function decorator to make f an RPC coroutine (to be offered).
  Adds it to the rpc table in this module's namespace, `rpcs`.
  Used to match a message's method to a function.
  Function signatures will get named (kw)args.
  See `handoff` for expected function signatures."
  (let [name f.__name__]
    (assoc rpcs f.__name__ f)
    f))

(defn :async handoff [payload]
  "Call the requested client async method (coroutine).
  Return result of `(await (method #** payload #** kwargs))`."
  (let [method (.pop payload "method" None)]
    (when (in method rpcs)
      (await ((get rpcs method) #** payload)))))

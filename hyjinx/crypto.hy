"
ECDSA public-private key signing and verification.
"

(require hyrule.argmove [-> ->>])

(import time [time])
(import hashlib [sha1 pbkdf2-hmac sha256])
(import hmac [compare-digest])
(import base64 [b64encode b64decode])
(import ecdsa [SigningKey VerifyingKey SECP256k1 util BadSignatureError])
(import ecdsa.util [sigencode-der sigdecode-der])


(defn hash-pw [pw]
  "Hash password with a secret salt."
  (let [salt (os.urandom 24)
        digest (pbkdf2-hmac "sha512"
                            (pw.encode "utf-8")
                            :iterations 100000
                            :salt salt)]
    {"salt" (.hex salt)
     "hexdigest" (.hex digest)}))

(defn check-pw [pw stored]
  "Check password is correct."
  (let [salt (bytes.fromhex (:salt stored))
        hexdigest (:hexdigest stored)]
    (compare-digest hexdigest (.hex (pbkdf2-hmac "sha512"
                                                 (pw.encode "utf-8")
                                                 :iterations 100000
                                                 :salt salt)))))

(defn signing-key [passphrase]
  (-> (passphrase.encode "utf-8")
      (sha256)
      (.hexdigest)
      (cut 32)
      (.encode "utf-8")
      (SigningKey.from-string :curve SECP256k1 :hashfunc sha256)))

(defn keys [passphrase]
  "PEM-encoded keys and private key object."
  (let [priv-key (signing-key passphrase)
        pub-key (.get-verifying-key priv-key)]
    {"private" priv-key
     "private_pem" (.decode (.to-pem priv-key :format "pkcs8") "utf-8")
     "public_pem" (.decode (.to-pem pub-key) "utf-8")}))

(defn sign [priv-key message]
  "Sign using private/signing key. Return (string) der signature."
  (let [bmsg (.encode message "utf-8")
        bsig (priv-key.sign-deterministic bmsg
                                          :hashfunc sha256
                                          :sigencode sigencode-der)]
    (.decode (b64encode bsig) "utf-8")))

(defn verify [pub-key-pem signature message]
  "Verify a (string) signature, (string) message pair. True if verified, None if bad."
  (let [bmsg (.encode message "utf-8")
        pub-key (.from-pem VerifyingKey pub-key-pem)
        bsig (b64decode signature)]
    (try
      (.verify pub-key bsig bmsg sha256 :sigdecode sigdecode-der)  
      (except [BadSignatureError]))))

(defn is-recent [client-time [threshold 120]]
  "Is client's message time within threshold (seconds) of server time?
  Used in message verification to avoid stale messages."
  (try
    (let [diff (abs (- (float client-time)
                       (time)))]
      (< diff threshold))
    (except [ValueError])))


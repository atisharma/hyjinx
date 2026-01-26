"
ECDSA (SECP256k1) stateless asymmetric signing, verification, and key derivation.

  For key derivation, PEM encoding, message signing/verification.
  No passphrase is sent in the clear.
  In-window replay attack mitigation should be handled at a higher layer using a nonce.

Credential storage and authentication.

  Salted password hashing for credential storage.
"

(require hyrule.argmove [-> ->>])
(require hyjinx.macros [defmethod])

(import base64 [b64encode b64decode])
(import ecdsa [SigningKey VerifyingKey SECP256k1 util BadSignatureError])
(import ecdsa.util [sigencode-der sigdecode-der])
(import hashlib [sha256 scrypt])
(import hmac [compare-digest])
(import os [urandom])
(import time [time])

;;; breaking changes in hyjinx 0.1.2
;;;   pbkdf2 -> scrypt for pw hashing
;;;   sha256 -> scrypt for key derivation
;;;   hash-pw salt length -> 32

;;; breaking changes in hyjinx 1.2.0
;;; arguments for keys() changed


;; trade-off between security and running on minimal hardware, e.g. pi 0
(setv scrypt-params {"n" 16384 "r" 8 "p" 1})


;; * Sender functions
;; ----------------------------------------------------

(defmethod signing-key [#^ str passphrase #^ str user-salt]
  "Derive the signing key from a passphrase and a salt.

  It is encourages to use a server-stored random salt.
  Returns a SigningKey object."
  (-> (passphrase.encode "utf-8")
      (scrypt #** scrypt-params
              :salt (.encode user-salt "utf-8")
              :dklen 32)
      (SigningKey.from-string :curve SECP256k1 :hashfunc sha256)))

(defmethod keys [#^ str passphrase #^ str user-salt]
  "Return dict of PEM-encoded keys and private key object."
  (let [priv-key (signing-key passphrase user-salt)
        pub-key (.get-verifying-key priv-key)]
    {"private" priv-key ; is a SigningKey
     "private_pem" (.decode (.to-pem priv-key :format "pkcs8") "utf-8")
     "public_pem" (.decode (.to-pem pub-key) "utf-8")}))

(defmethod sign [#^ SigningKey priv-key #^ str message]
  "Sign using private/signing key.
  Return (string) der signature."
  (let [bmsg (.encode message "utf-8")
        bsig (priv-key.sign-deterministic bmsg
                                          :hashfunc sha256
                                          :sigencode sigencode-der)]
    (.decode (b64encode bsig) "utf-8")))


;; * Receiver functions
;; ----------------------------------------------------

(defmethod verify [#^ str pub-key-pem #^ str signature #^ str message]
  "Verify a signature, message pair.

  Return True if verified, False if bad."
  (let [bmsg (.encode message "utf-8")
        pub-key (.from-pem VerifyingKey pub-key-pem)
        bsig (b64decode signature)]
    (try
      (.verify pub-key bsig bmsg sha256 :sigdecode sigdecode-der)  
      True
      (except [BadSignatureError]
        False))))

(defmethod is-recent [#^ float client-time #^ float [threshold 30.0]]
  "Client's message time is within threshold (seconds) of server time.

  Used in message verification to avoid stale messages.
  This is for timeliness of messaging, not to prevent replay attacks.
  Return True or False."
  (try
    (let [diff (abs (- client-time
                       (time)))]
      (< diff threshold))
    (except [ValueError TypeError]
      False)))


;; * Credential verification
;; ----------------------------------------------------

(defmethod hash-pw [#^ str pw]
  "Hash password with a secret salt."
  (let [salt (urandom 32)
        digest (scrypt (pw.encode "utf-8")
                       #** scrypt-params
                       :salt salt
                       :dklen 64)]
    {"salt" (.hex salt)
     "hexdigest" (.hex digest)}))

(defmethod check-pw [#^ str pw #^ dict stored]
  "Check password is correct against stored salted digest."
  (let [salt (.fromhex bytes (:salt stored))
        hexdigest (:hexdigest stored)]
    (compare-digest hexdigest (.hex (scrypt (pw.encode "utf-8")
                                            #** scrypt-params
                                            :salt salt
                                            :dklen 64)))))

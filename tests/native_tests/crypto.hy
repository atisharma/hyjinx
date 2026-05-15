"
Tests for hyjinx.crypto — ECDSA signing, verification, and password hashing.
"

(import time [time])
(import hyjinx.crypto)


;; ── Key derivation ────────────────────────────────────────────────────────────

(defn test-signing-key-returns-signing-key []
  (let [sk (hyjinx.crypto.signing-key "passphrase" "salt123")]
    (assert (isinstance sk hyjinx.crypto.SigningKey))))

(defn test-signing-key-deterministic []
  (let [sk1 (hyjinx.crypto.signing-key "passphrase" "salt123")
        sk2 (hyjinx.crypto.signing-key "passphrase" "salt123")]
    (assert (= (.to-string sk1) (.to-string sk2)))))

(defn test-signing-key-different-salts []
  (let [sk1 (hyjinx.crypto.signing-key "passphrase" "salt1")
        sk2 (hyjinx.crypto.signing-key "passphrase" "salt2")]
    (assert (!= (.to-string sk1) (.to-string sk2)))))


;; ── Keys ──────────────────────────────────────────────────────────────────────

(defn test-keys-returns-dict []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")]
    (assert (isinstance ks dict))
    (assert (in "private" ks))
    (assert (in "private_pem" ks))
    (assert (in "public_pem" ks))))

(defn test-keys-private-is-signing-key []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")]
    (assert (isinstance (:private ks) hyjinx.crypto.SigningKey))))

(defn test-keys-pem-are-strings []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")]
    (assert (isinstance (:private_pem ks) str))
    (assert (isinstance (:public_pem ks) str))))

(defn test-keys-pem-starts-with-headers []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")]
    (assert (.startswith (:private_pem ks) "-----BEGIN"))
    (assert (.startswith (:public_pem ks) "-----BEGIN"))))


;; ── Sign and verify ───────────────────────────────────────────────────────────

(defn test-sign-returns-string []
  (let [sk (hyjinx.crypto.signing-key "passphrase" "salt123")
        sig (hyjinx.crypto.sign sk "hello")]
    (assert (isinstance sig str))))

(defn test-sign-verify-roundtrip []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")
        sig (hyjinx.crypto.sign (:private ks) "test message")]
    (assert (hyjinx.crypto.verify (:public_pem ks) sig "test message"))))

(defn test-verify-rejects-tampered-message []
  (let [ks (hyjinx.crypto.keys "passphrase" "salt123")
        sig (hyjinx.crypto.sign (:private ks) "original")]
    (assert (not (hyjinx.crypto.verify (:public_pem ks) sig "tampered")))))

(defn test-verify-rejects-wrong-key []
  (let [ks1 (hyjinx.crypto.keys "pass1" "salt1")
        ks2 (hyjinx.crypto.keys "pass2" "salt2")
        sig (hyjinx.crypto.sign (:private ks1) "message")]
    (assert (not (hyjinx.crypto.verify (:public_pem ks2) sig "message")))))

(defn test-sign-different-messages-different-signatures []
  (let [sk (hyjinx.crypto.signing-key "passphrase" "salt123")
        sig1 (hyjinx.crypto.sign sk "message one")
        sig2 (hyjinx.crypto.sign sk "message two")]
    (assert (!= sig1 sig2))))


;; ── is-recent ─────────────────────────────────────────────────────────────────

(defn test-is-recent-with-current-time []
  (assert (hyjinx.crypto.is-recent (time))))

(defn test-is-recent-with-old-time []
  (assert (not (hyjinx.crypto.is-recent (- (time) 60)))))

(defn test-is-recent-custom-threshold []
  (assert (hyjinx.crypto.is-recent (- (time) 20) :threshold 30.0))
  (assert (not (hyjinx.crypto.is-recent (- (time) 40) :threshold 30.0))))

(defn test-is-recent-invalid-input []
  ;; defmethod dispatches on float, so non-float raises DispatchError
  (import multimethod [DispatchError])
  (try
    (hyjinx.crypto.is-recent None)
    (assert False "should have raised")
    (except [DispatchError]
      (assert True))))


;; ── Password hashing ──────────────────────────────────────────────────────────

(defn test-hash-pw-returns-dict []
  (let [result (hyjinx.crypto.hash-pw "secret")]
    (assert (isinstance result dict))
    (assert (in "salt" result))
    (assert (in "hexdigest" result))))

(defn test-hash-pw-salt-is-hex []
  (let [result (hyjinx.crypto.hash-pw "secret")]
    ;; 32 bytes = 64 hex chars
    (assert (= (len (:salt result)) 64))))

(defn test-hash-pw-different-salts []
  (let [h1 (hyjinx.crypto.hash-pw "secret")
        h2 (hyjinx.crypto.hash-pw "secret")]
    (assert (!= (:salt h1) (:salt h2)))
    (assert (!= (:hexdigest h1) (:hexdigest h2)))))

(defn test-check-pw-correct []
  (let [stored (hyjinx.crypto.hash-pw "correct-password")]
    (assert (hyjinx.crypto.check-pw "correct-password" stored))))

(defn test-check-pw-wrong []
  (let [stored (hyjinx.crypto.hash-pw "correct-password")]
    (assert (not (hyjinx.crypto.check-pw "wrong-password" stored)))))

(defn test-check-pw-different-hashes-same-password []
  (let [stored1 (hyjinx.crypto.hash-pw "same-password")
        stored2 (hyjinx.crypto.hash-pw "same-password")]
    ;; Different salts but both should verify
    (assert (hyjinx.crypto.check-pw "same-password" stored1))
    (assert (hyjinx.crypto.check-pw "same-password" stored2))))

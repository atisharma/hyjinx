"
Send an email using TLS/SSL.
"

(require hyrule [defmain])

(import email.message [EmailMessage])
(import smtplib)
(import sys)
(import traceback)

(import hyjinx.lib [config])

;; could use idna for funky intl domain names


(defclass MailError [RuntimeError])


(defn send [body *
            #^ str subject
            #^ str sender 
            #^ str recipient 
            #^ str server 
            #^ int port 
            #^ str password 
            #^ str username
            #^ float [timeout 60.0]]
  "Send an email.
  
  Sends the body (which may be unicode text) using the specified SMTP server.
  
  kwargs:
    - subject: Email subject line (required)
    - sender: Sender email address
    - recipient: Recipient email address
    - server: SMTP server hostname
    - port: SMTP server port
    - password: SMTP authentication password
    - username: Used for login authentication
    - timeout: Connection timeout (default 60s)
  "
  (try
    (let [msg (EmailMessage)]
      (.set-content msg body)
      (setv (get msg "Subject") subject
            (get msg "From") sender
            (get msg "To") recipient)
      (with [server (smtplib.SMTP_SSL server port :timeout timeout)]
        (.login server username password)
        (.send-message server msg)))
    (except [e Exception]
      (let [error-msg (str e)
            sanitized-error-msg (.replace error-msg password (* "*" (len password)))]
        (raise (MailError sanitized-error-msg) :from None)))))

(defn send-exception [exc * #^ str [subject "error"] #** kwargs]
  "Send a formatted Exception object.
  Requires the same kwargs as `send`."
  (let [tb (.format-exception traceback
                              :etype (type exc)
                              :value exc
                              :tb exc.__traceback__)
        tb-str (.join "" tb)]
    (send tb-str :subject subject #** kwargs)))

(defmain [prog-name config-file recipient subject]
  "Send content from stdin as an email.
  
  Reads the email body from standard input and sends it using the
  configured SMTP server. The subject must be provided via keyword
  argument.
  
  Args:
    - config-file: A toml file containing a `mail` section with
      values for:
      * server: SMTP server,
      * port: the SMTP port,
      * username: the username for authentication,
      * password: the authentication password,
      * sender: the sender's email.
    - subject: Email subject line (required)
    - recipient: Recipient email address
  "
  (try
    (send (.read sys.stdin)
      :subject subject
      :recipient recipient
      #** (:mail (config config-file)))
    (except [e [Exception]]
      (print f"Error sending mail: {e}" :file sys.stderr)
      (sys.exit 1))))

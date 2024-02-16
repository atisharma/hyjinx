"
A Large Language Model in your repl.
"

(require hyrule [-> ->> unless])
(require hyjinx.macros [defmethod])

(import hyrule [pformat])
(import hyjinx.lib [first last])
(import hyjinx.source [get-source get-source-details])

(import httpx
        shutil)
(import types [ModuleType FunctionType MethodType TracebackType])
(import multimethod [multimethod])
(import openai [OpenAI])
(import json.decoder [JSONDecodeError])


(defclass TabbyClientError [Exception])
(defclass ChatError [Exception])

(setv HasCodeType (| type ModuleType FunctionType MethodType TracebackType))

;; * the actually useful functions 
;; -----------------------------------------------------------------------------

(defmethod discuss [#^ OpenAI client #^ str prompt *
                    [print True]
                    [margin "  "]
                    [width None]
                    [max-tokens 500]
                    #** kwargs]
  "Just ask a general question, no object, no chat."
  (let [sys (_system "You are an intelligent assistant.")
        usr (_user prompt)
        stream (_completion client [sys usr] :max-tokens max-tokens #** kwargs)]
    (_output stream :print print :width width :margin margin)))

(defmethod discuss [#^ OpenAI client #^ str prompt #^ HasCodeType obj *
                    [print True]
                    [margin "  "]
                    [width None]
                    [max-tokens 500]
                    #** kwargs]
  "Discuss a hy or python object's source code."
  (let [details (get-source-details obj)
        language (:language details)
        source (get-source obj)
        sys (_system "You are an intelligent programming assistant.")
        usr (_user f"{prompt}\nHere is the {language} code for {obj}:\n{source}")
        stream (_completion client [sys usr] :max-tokens max-tokens #** kwargs)]
    (_output stream :print print :width width :margin margin)))

(defmethod explain [#^ OpenAI client #^ HasCodeType obj * [print True] #** kwargs]
  "Explain the purpose of a hy or python object from its source code."
  (discuss client f"Clearly explain the purpose of the following code." obj :print print #** kwargs))

(defmethod docstring [#^ OpenAI client #^ HasCodeType obj * [print True] #** kwargs]
  "Write a docstring for an object from its source code."
  (discuss client f"Write a good docstring for the following code." obj :print print #** kwargs))

(defmethod write-test [#^ OpenAI client #^ HasCodeType obj * [print True] #** kwargs]
  "Write a test for an object from its source code."
  (discuss client f"Write a good test for the following code." obj :print print #** kwargs))

;; * message convenience functions
;; -----------------------------------------------------------------------------

(defn _msg [#^ str role
            #^ str content]
  "Just a simple dict with the needed fields."
  (if content
      {"role" role
       "content" (.strip content)}
      (raise (ChatError f"No content in message (role: {role})."))))

(defn _system [#^ str content]
  (_msg "system" content))

(defn _user [#^ str content]
  (_msg "user" content))

(defn _assistant [#^ str content]
  (_msg "assistant" content))

;; * output handling
;; -----------------------------------------------------------------------------

(defn _output [stream * [print True] #** kwargs]
  (if print
      (_print-stream stream #** kwargs)
      (.join "" stream)))

(defn _print-stream [stream * [width None] [margin "  "]]
  "Print a streaming chat completion."
  (let [term (shutil.get-terminal-size)]
    (setv line "")
    (print margin :end "")
    (for [chunk stream :if (-> chunk.choices
                                (first)
                                (getattr "delta")
                                (getattr "content"))]
      (let [choice (first chunk.choices)
            content choice.delta.content]
        (+= line content)
        (cond (.endswith content "\n")
              (do
                (print f"{content}{margin}" :end "")
                (setv line ""))

              (> (+ (len line) (len margin))
                 (or width (- term.columns 5)))
              (do (print f"\n{margin}{(.strip content)}" :end "")
                  (setv line (.strip content)))

              :else
              (print content :end "" :flush True))))
    (print)))

;; * the Tabby API client
;; ----------------------------------------------------

(defclass TabbyClient [OpenAI]
  "A REPL-facing client for TabbyAPI."

  (defn __init__ [self #** kwargs]
    "base-url should have the 'v1' at the end."
    (setv self.admin_key (.pop kwargs "admin_key" None))
    (.__init__ (super) #** kwargs))

  (defn _get [self endpoint]
    "GET an authenticated endpoint or raise error."
    (let [response (httpx.get (.join self.base-url endpoint)
                              :headers {"x-api-key" self.api-key})]
      (if response.is-success
          (response.json)
          (raise (TabbyClientError f"{response.status-code}\n{(pformat (:detail (response.json)) :indent 2)}")))))

  (defn _post [self endpoint * [admin False] #** data]
    "POST to an authenticated endpoint or raise error."
    (let [auth (if admin
                   {"x-admin-key" self.admin-key}
                   {"x-api-key" self.api-key})
          response (httpx.post (.join self.base-url endpoint)
                               :headers auth
                               :json (or data {}))]
      (if response.is-success
          (try
            (.json response)
            (except [e [JSONDecodeError TypeError]]
              response))
          (raise (TabbyClientError f"{response.status-code}\n{(pformat (:detail (response.json)) :indent 2)}"))))))

;; * generation methods requiring user authentication
;; ----------------------------------------------------

(defmethod _completion [#^ OpenAI client messages * [stream True] #** kwargs]
  "Generate a streaming completion using the chat completion endpoint.
  In python, use as
        for chunk in stream:
            print(chunk.choices[0].delta.content or '', end='') "
  (client.chat.completions.create
    :model (.pop kwargs "model" (getattr client "model" "gpt-3.5-turbo"))
    :messages messages
    :stream stream
    #** kwargs))

;; * methods requiring user authentication
;; ----------------------------------------------------

(defmethod models [#^ TabbyClient client]
  "List all models available to TabbyAPI."
  (let [l (:data (client._get "models"))]
    (sorted (lfor m l (:id m)))))

(defmethod loras [#^ TabbyClient client]
  "List all loras available to TabbyAPI."
  (:data (client._get "loras")))

(defmethod models [#^ OpenAI client]
  "List all models available to OpenAI."
  (let [models (client.models.list)]
    (lfor m models m.id)))

(defmethod model [#^ TabbyClient client]
  "Get the currently loaded model."
  (client._get "model"))
    
(defmethod lora [#^ TabbyClient client]
  "Get the currently loaded loras."
  (:data (.json (client._get "lora"))))

(defmethod templates [#^ TabbyClient client]
  "List all templates available to TabbyAPI."
  (sorted (:data (client._get "templates"))))

;; * methods requiring admin authentication
;; ----------------------------------------------------

(defmethod template-load [#^ TabbyClient client #^ str template]
  "Set the currently loaded template."
  (client._post "template/switch"
                :admin True
                :name template))

(defmethod template-unload [#^ TabbyClient client]
  "Unload the currently loaded template."
  (client._post "template/unload"
                :admin True))

(defmethod model-load [#^ OpenAI client #^ str model #** kwargs]
  "Set the OpenAI model to use for completions.
  The OpenAI class expects the model to be specified in completions, so this sets a default."
  (setv client.model model))

(defmethod model-load [#^ TabbyClient client #^ str model #** kwargs]
  "Load a model.
  TabbyAPI needs to load/unload models before use.
  kwargs are passed to the API.
  See the TabbyAPI docs for valid keys and values."
  ;; TODO : steam response to show progress
  (client._post "model/load"
                :admin True
                :name model
                #** kwargs))

(defmethod model-unload [#^ TabbyClient client]
  "Unload a model.
   TabbyAPI needs to load/unload models before use."
  (client._post "model/unload"
                :admin True))

(defmethod lora-load [#^ TabbyClient client #^ list loras]
  "Load LoRas when using TabbyAPI.
  loras is a list of dicts, with 'name', 'scaling' as keys."
  (client._post "lora/load"
                :admin True
                :loras loras))
    
(defmethod lora-unload [#^ TabbyClient client]
  "Unload LoRas when using TabbyAPI."
  (client._post "lora/unload"
                :admin True))

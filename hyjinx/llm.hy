"
A Large Language Model in your REPL.

APIs and utilities for interacting with a Large Language Model (LLM)
assistant in Hy.

This module provides functions to interact with a Large Language Model
assistant for various tasks such as generating code comments,
providing explanations, performing code review, and rewriting code. It
includes a Hy implementation of a TabbyClient to interact with
TabbyAPI for model management and streaming completions. It also
supports various LLM providers such as OpenAI and Anthropic.

Features:
- converse: Chat over a list of messages, updating the message list in-place.
- definstruct: A macro to create functions for specific tasks like
  comments, docstrings, explanations, review, rewrite, and test
  (these examples have been implemented)
- instruct: Generic instruction method for the assistant.
- TabbyClient: Implementation of a client to interact with TabbyAPI for model management and
  streaming completions.
- latex highlighting, if pdflatex, dvipng and img2sixel are installed and you're using a
  sixel-capable terminal.

Functions:
- `converse`: Chat over a list of messages and update in-place the message list.
- `definstruct`: Create methods for instructing over python/hy objects.
- `instruct`: Generic instruction method for the assistant.
- Various methods for managing TabbyAPI servers, such as loading models, templates, LoRAs
  and configuring the client.

The TabbyClient class:
- `models`: List all models available to TabbyAPI or OpenAI.
- `model-load`: Set the currently loaded model (TabbyAPI).
- `model-unload`: Unload the currently loaded model (TabbyAPI).
- `templates`: List all templates available to TabbyAPI.
- `template-load`: Set the currently loaded template (TabbyAPI).
- `template-unload`: Unload the currently loaded template (TabbyAPI).
- `loras`: List all LoRAs available to TabbyAPI.
- `lora-load`: Load LoRAs when using TabbyAPI.
- `lora-unload`: Unload LoRA when using TabbyAPI.

Example usage:

```hylang
(import hyjinx [llm])
(import yaml)

(setv auth (with [f (open  \"/srv/tabby-api/api_tokens.yml\" \"r\")]
             (yaml.safe-load f)))
(setv tabby (llm.TabbyClient :base-url \"http://localhost:5000/v1\" #** auth))

; tidy up the namespace
(del yaml)
(del auth)

; set up a chat with memory
(setv tchat (Chat :client tabby :system-prompt \"Act like an unhinged pirate.\"))

; load a code-literate model
(llm.model-load tabby \"CodeFuse-DeepSeek-33B-6.0bpw-h6-exl\")
(llm.template-load tabby \"alpaca\")

; get the llm to explain the llm module
(llm.explain tabby llm)
```
"

(import re)
(import os)
(import subprocess)
(import shutil)
(import tempfile [TemporaryDirectory])
(import base64)

(require hyrule [-> ->> unless of])
(require hyjinx.macros [defmethod rest lmap])

(import hyrule [pformat])
(import hyjinx.lib [first last hash-color spit filetype])
(import hyjinx.inspect [getsource])
(import hyjinx.source [get-source-details])
(import pygments)
(import pygments [highlight])
(import pygments.lexers [get-lexer-by-name guess-lexer])
(import pygments.formatters [TerminalFormatter])

(import magic)
(import httpx)
(import types [ModuleType FunctionType MethodType TracebackType])
(import itertools [tee])
(import json.decoder [JSONDecodeError])
(import pansi [ansi :as _ansi])

(try
  (import openai [OpenAI])
  (except [ModuleNotFoundError]
    (defclass OpenAI)))

(try
  (import anthropic [Anthropic])
  (except [ModuleNotFoundError]
    (defclass Anthropic)))


(defclass TabbyClientError [Exception])
(defclass ChatError [Exception])

(setv HasCodeType (| type ModuleType FunctionType MethodType TracebackType))
(setv ClientType (| OpenAI Anthropic))
(setv ContentType (| str (of list dict)))

;; * the actually useful functions 
;; -----------------------------------------------------------------------------

(defmethod converse [client #^ (of list dict) messages #^ ContentType content *
                     [system-prompt None]
                     [color None]
                     #** kwargs]
  "Chat over a list of messages and update in-place the message list.
  The system prompt (if any) is injected each call so can be changed.

  You might use this function like:
  (setv _messages [])
  (setv chat (partial converse tabby _messages))
  (chat \"Hello there.\")
  
  The supplied content can be a string,
  or a list of dicts with text and a base64 encoded image (see ContentType, image-content)."
  (let [usr (_user content)
        [output-1 output-2] (tee (_completion
                                   client
                                   (if system-prompt
                                     [(_system system-prompt)
                                      #* messages
                                      usr]
                                     [#* messages usr])
                                   #** kwargs))]
    (_output output-1
             :print True
             :color (or color (hash-color (or client.model ""))))
    (.append messages usr)
    (.append messages (_assistant (_output output-2 :print False)))))

(defclass Chat []
  "Create a callable that chats with the client over a stored message list.

  This class encapsulates a chat session, maintaining a history of messages and
  providing methods to interact with a client. It can be initialized with an
  optional client, system prompt, and color, allowing customization of the
  chat's appearance and behavior. The class supports image-based messages and
  maintains a history of conversations which can be retrieved using the __str__
  method.
  "

  (defn __init__ [self [client None] * [system-prompt None] [color None]]
    (setv self._client client)
    (setv self._system-prompt system-prompt)
    (setv self._messages [])
    (setv self._color color))

  (defn __call__ [self text * [image None] [client None] [color None] #** kwargs]
    "Handles the chat interaction by appending the user message and API response
    to the stored list."
    (converse (or client self._client)
              self._messages
              (if image
                (image-content (or client self._client) text image)
                text)
              :system-prompt self._system-prompt
              :color (or color self._color)
              #** kwargs))

  (defn __str__ [self]
    "Pretty-prints the chat history with roles in deterministic colors."
    (import pansi [ansi])
    (.join "\n\n"
      [(str client)
       #* (lfor m self._messages
            (let [role (:role m)
                  color (hash-color role)
                  content (if (isinstance (:content m) str)
                            (:content m)
                            (.join "\n" (lfor t (:content m) (:text t "<b64encoded image>"))))]
              f"\t{color}{_ansi.b}{role}{_ansi._b}\n{content}"))
       _ansi.reset])))

(defmacro definstruct [f prompt]
  "Create a function that instructs over a python/hy object."
  `(defn ~f [client obj * [print True] #** kwargs]
     ~prompt
     (instruct client
              ~prompt
              obj
              :print print
              #** kwargs)))

(defmethod instruct [client #^ str prompt *
                     [image None]
                     [print True]
                     [system-prompt None]
                     [color _ansi.reset]
                     #** kwargs]
  "Just ask a general instruction or question, no object, no chat."
  (let [usr (if image
              (_user (image-content client prompt image))
              (_user prompt))
        msgs (if system-prompt
               [(_system system-prompt) usr]
               [usr])
        stream (_completion client msgs #** kwargs)]
    (_output stream :print print :color color)))

(defmethod instruct [client #^ str prompt #^ HasCodeType obj *
                     [print True]
                     [system-prompt None]
                     [color _ansi.reset]
                     #** kwargs]
  "Instruct a hy or python object's source code."
  (let [details (get-source-details obj)
        language (:language details)
        source (getsource obj)
        usr (_user f"You will play the part of an intelligent, expert and concise senior programmer. You will be shown code for {obj} (module {(:module details)}). It is in the {language} language.
{prompt}

{source}")
        stream (_completion client [sys usr] #** kwargs)
        msgs (if system-prompt
               [(_system system-prompt) usr]
               [usr])]
    (_output stream :print print :color color)))

;; TODO use template files like pugsql

(definstruct comments "Rewrite the following code, with high-quality comments.")

(definstruct docstring "Write a high-quality docstring for the following code.")

(definstruct explain "Clearly explain the purpose of the following code.")

(definstruct review "Write a clear, high-quality peer-review of the following code. Identify any bugs, logic errors, and overlooked edge cases. Briefly explain your reasoning. Concentrate on the most important points first.")

(definstruct rewrite "Rewrite the following code in the same language, improving quality and clarity. Briefly state what you will do before giving the rewritten code.")

(definstruct test "Write a high-quality test for the following code.")

(definstruct example "Give a minimal and correct example usage for the following code.")

;; * message convenience functions
;; -----------------------------------------------------------------------------

(defmethod image-content [#^ OpenAI client #^ str text #^ str image-path]
  "Create a message for OpenAI-compatible API with text and image content."
  (if image-path
    [{"type" "text" "text" text}
     {"type" "image_url"
      "image_url" {"url" (+ f"data:{(:mime (filetype image-path))};base64,"
                            (b64-encode-image image-path))}}]
    text))

(defmethod image-content [#^ Anthropic client #^ str text #^ str image-path]
  "Create a message for Anthropic API with text and image content.
  Currently, Claude supports image/jpeg, image/png, image/gif, image/webp."
  ;; TODO: list of multiple images
  (if image-path
    [{"type" "text" "text" text}
     {"type" "image"
      "source" {"type" "base64"
                "media_type" (:mime (filetype image-path))
                "data" (b64-encode-image image-path)}}]
    text))

(defn _msg [#^ str role #^ ContentType content]
  "Just a simple dict with the needed fields.
  The supplied content can be a string or a list of dicts with text and a base64 encoded image."
  (if content
      {"role" role
       "content" content}
      (raise (ChatError f"No content in message (role: {role})."))))

(defn _system [#^ ContentType content]
  (_msg "system" content))

(defn _user [#^ ContentType content]
  (_msg "user" content))

(defn _assistant [#^ ContentType content]
  (_msg "assistant" content))

;; * output handling
;; -----------------------------------------------------------------------------

(defn _start-code-fence [s]
  "Check if the stream chunk starts a code block.
  Uses either three backticks or three tildes after a newline, with optional language specifier."
  (re.search r"(```|~~~)\s*([\w-]+)?[\r\n]+(.*)" s)) ; Claude doesn't split chunks at end of line
  
(defn _end-code-fence [s]
  "Check if the stream chunk ends a code block.
  Uses either three backticks or three tildes after a newline or newline then whitespace."
  (re.search r"[\n\r\f][\s]*(```|~~~)[\n\r\f]+(.*)" s))  ; Claude doesn't split chunks at end of line, Mistral likes to indent code fences.

(defn _latex-expr [s]
  "Check if the stream chunk ends a latex expression. Uses \\( and \\[."
  ;; see https://stackoverflow.com/questions/14182879/regex-to-match-latex-equations
  ;; and https://regex101.com/r/wP2aV6/25
  (re.search r"
(?<!\\)    # negative look-behind to make sure start is not escaped 
(?:        # start non-capture group for all possible match starts
  # group 1, match dollar signs only 
  # single or double dollar sign enforced by look-arounds
  ((?<!\$)\${1,2}(?!\$))|
  # group 2, match escaped parenthesis
  (\\\()|
  # group 3, match escaped bracket
  (\\\[)|                 
  # group 4, match begin equation
  (\\begin\{equation\})
)
# if group 1 was start
(?(1)
  # non greedy match everything in between
  # group 1 matches do not support recursion
  (.*?)(?<!\\)
  # match ending double or single dollar signs
  (?<!\$)\1(?!\$)|  
# else
(?:
  # greedily and recursively match everything in between
  # groups 2, 3 and 4 support recursion
  #(.*(?R)?.*)(?<!\\)
  (.?.*)(?<!\\)
  (?:
    # if group 2 was start, escaped parenthesis is end
    (?(2)\\\)|  
    # if group 3 was start, escaped bracket is end
    (?(3)\\\]|     
    # else group 4 was start, match end equation
    \\end\{equation\}
  )
))))
  " s re.X))

(defn _output [stream * [print True] #** kwargs]
  (if print
      (_print-stream stream #** kwargs)
      (.join "" stream)))

(defn _fprint [s]
  (print s :flush True :end ""))

(defn _print-stream [stream * [bg "dark"] [color _ansi.reset]]
  "Print a streaming chat completion.
  Applies syntax highlighting to the string inside a code fence.
  Any syntax-highlighted strings are printed as streamed."
  (let [sixel (and (shutil.which "pdflatex")
                   (shutil.which "dvipng")
                   (shutil.which "img2sixel"))]
    ;; we will accumulate streamed content into code-text and latex-text
    (setv code-text "")
    (setv latex-text "")
    (_fprint color)

    (for [content stream]
      (+= code-text content)
      (+= latex-text content)
      (let [code-match (_start-code-fence code-text) ; check on each streamed token
            latex-match (when sixel (_latex-expr latex-text))]
        (cond

          code-match
          (do
            (print (cut content (- (len (code-match.group 3))))) ; the bit before the opening fence, add newline lost in regex
            (setv code-text "")
            ;; what isn't already printed goes to the other function
            (let [trailing (_print-code-block stream
                                              :bg bg 
                                              :fence (code-match.group 1) 
                                              :lang (code-match.group 2) 
                                              :code (code-match.group 3))]
              (_fprint (+ color trailing color))))

          latex-match
          (do
            (_fprint content)
            (_print-latex-expr (latex-match.group 0) :bg bg)
            (_fprint color)
            (setv latex-text (.join " " (rest (.split content))))) ; manage the same problem of trailing tokens

          :else
          (_fprint content))))

    (print _ansi.reset)))

(defn _print-code-block [stream * [fence "```"] [lang ""] [code ""] [bg "dark"]]
  "Applies syntax highlighting to a streaming chat completion,
  accumulating strings then breaking and printing when exiting the code fence."
  (setv text "") ; after the closing fence
  (for [content stream]
    (+= code content) ; accumulate code lines for highlighting
    (let [match (_end-code-fence code)]
      (when match ; check after each streamed token, print when done
        (let [l (len (match.group 0))]
          ;; remove trailing non-code stuff and save it for later
          ;; take care using cut when (len code) is 0.
          (setv text (cut code (- (len code) l) None))
          (setv code (cut code (- (len code) l)))
          (break)))))
  (let [formatter (TerminalFormatter :bg bg :stripall True)
        ;lines (.split code "\n")
        lexer (if lang
                (try
                  (get-lexer-by-name lang)
                  (except [pygments.util.ClassNotFound]
                    (guess-lexer code)))
                (guess-lexer code))]
    (_fprint (highlight code lexer formatter))
    text)) ; return any trailing text

(defn _print-latex-expr [expr * [bg "dark"] [dpi "200"] [timeout 4]]
  "Shell out to print sixel generated from a latex expression."
  ;; see latex2sixel, https://github.com/nilqed/latex2sixel/
  (with [tmpdir (TemporaryDirectory)]
    (let [tex-str (.join "\n" [r"\documentclass[12pt]{article}"
                               r"\usepackage{amsmath,amssymb}"
                               r"\usepackage{breqn}"
                               r"\pagestyle{empty}"
                               r"\begin{document}"
                               expr
                               r"\end{document}"])
          tex (os.path.join tmpdir "hyjinx.tex")
          png (os.path.join tmpdir "hyjinx.png")
          dvi (os.path.join tmpdir "hyjinx.dvi")
          logfile (os.path.join tmpdir "hyjinx.log")]
      (spit tex tex-str)
      (subprocess.run ["pdflatex"
                       "-output-format=dvi"
                       ;"-jobname=hyjinx"
                       f"-output-directory={tmpdir}"
                       "-interaction=nonstopmode"
                       tex]
                      :capture-output True
                      :encoding "utf-8"
                      :timeout timeout)
      (subprocess.run ["dvipng" 
                       "-T" "bbox" 
                       "-D" (str 200) ; we are guessing the DPI
                       "-O" "-1.5cm,-1.8cm" 
                       "-q" 
                       f"-o" png
                       "-fg" (if (= bg "dark") "White" "Black")
                       "-bg" (if (= bg "dark") "Black" "White")
                       dvi] 
                      :capture-output True
                      :timeout timeout)  
      (print :flush True)
      (subprocess.run ["img2sixel" png] :capture-output False :timeout timeout))))

;; * the Tabby API client
;; ----------------------------------------------------

(defclass TabbyClient [OpenAI]
  "A REPL-facing client for TabbyAPI (https://github.com/theroyallab/tabbyAPI).

  The `TabbyClient` class provides a client to interact with TabbyAPI
  for model management and streaming completions. It provides methods
  to load, unload, and manage models, as well as other useful features
  such as streaming completions and handling templates and LoRAs. An
  admin key is required for some endpoints.

  Methods are defined at the module level."

  (defn __init__ [self #** kwargs]
    "The base-url should have the 'v1' at the end.
     Initialise as for OpenAI, but optionally pass admin_key as well."
    (setv self.model None)
    (setv self._admin_key (.pop kwargs "admin_key" None))
    (.__init__ (super) #** kwargs))

  (defn _get [self endpoint * [timeout 20]]
    "GET an authenticated endpoint or raise error."
    (let [response (httpx.get (.join self.base-url endpoint)
                              :headers {"x-api-key" self.api-key}
                              :timeout timeout)]
      (if response.is-success
          (response.json)
          (raise (TabbyClientError f"{_ansi.red}{response.status-code}\n{(pformat (:detail (response.json)) :indent 2)}{_ansi.reset}")))))

  (defn _post [self endpoint * [admin False] [timeout 20] #** data]
    "POST to an authenticated endpoint or raise error."
    (let [auth (if admin
                   {"x-admin-key" self._admin-key}
                   {"x-api-key" self.api-key})
          response (httpx.post (.join self.base-url endpoint)
                               :headers auth
                               :json (or data {})
                               :timeout timeout)]
      (if response.is-success
          (try
            (.json response)
            (except [e [JSONDecodeError TypeError]]
              response))
          (raise (TabbyClientError f"{_ansi.red}{response.status-code}\n{(pformat (:detail (response.json)) :indent 2)}{_ansi.reset}"))))))

;; * generation methods requiring user authentication
;; ----------------------------------------------------

(defmethod _completion [#^ OpenAI client messages * [stream True] [max-tokens 1000] #** kwargs]
  "Generate a streaming completion using the chat completion endpoint."
  (let [stream (client.chat.completions.create
                 :model (.pop kwargs "model" (getattr client "model" "gpt-4-turbo"))
                 :messages messages
                 :stream stream
                 :max-tokens max-tokens
                 #** kwargs)]
    (for [chunk stream :if chunk.choices]
      (let [text (. (. (first chunk.choices) delta) content)]
        (when text
          (yield text))))))

(defmethod _completion [#^ Anthropic client messages * [stream True] [max-tokens 1000] #** kwargs]
  "Generate a streaming completion using the messages endpoint."
  (let [system-messages (.join "\n"
                               (lfor m messages
                                     :if (= (:role m) "system")
                                     (:content m)))
        messages (lfor m messages
                       :if (not (= (:role m) "system"))
                       m)]
    (with [stream (client.messages.stream
                    :model (.pop kwargs "model" (getattr client "model" "claude-3-opus"))
                    :system system-messages
                    :messages messages
                    :max-tokens max-tokens
                    #** kwargs)]
      (for [text stream.text-stream :if text]
        (yield text)))))

;; * methods requiring user authentication
;; ----------------------------------------------------

(defmethod models [#^ OpenAI client]
  "List all models available to OpenAI."
  (let [models (client.models.list)]
    (lfor m models m.id)))

(defmethod models [#^ TabbyClient client]
  "List all models available to TabbyAPI."
  (let [l (:data (client._get "models"))]
    (sorted (lfor m l (:id m)))))

(defmethod loras [#^ TabbyClient client]
  "List all loras available to TabbyAPI."
  (let [loras (:data (client._get "loras"))]
    (sorted (lfor l loras {"name" (:id l) #** l})
            :key :name)))

(defmethod model [#^ OpenAI client]
  "Get the currently selected model."
  {"id" client.model})

(defmethod model [#^ TabbyClient client]
  "Get the currently loaded model."
  (let [response (client._get "model")]
    (setv client.model (:id response))
    response))
    
(defmethod lora [#^ TabbyClient client]
  "Get the currently loaded loras."
  (client._get "lora"))

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

(defmethod model-load [client #^ str model #** kwargs]
  "Set the OpenAI or Anthropic model to use for completions.
  The OpenAI or Anthropic class expects the model to be specified in completions.
  This just sets a default."
  (setv client.model model))

(defmethod model-load [#^ TabbyClient client #^ str model #** kwargs]
  "Load a model.
  TabbyAPI needs to load/unload models before use.
  kwargs are passed to the API.
  See the TabbyAPI docs for valid keys and values, for example, to load
  draft models for speculative decoding."
  ;; TODO : stream response to show progress
  (let [response (client._post "model/load"
                               :admin True
                               :name model
                               :timeout None
                               #** kwargs)]
    (setv client.model model)
    (print f"{model} loaded.")))

(defmethod model-unload [#^ TabbyClient client]
  "Unload a model.
   TabbyAPI needs to load/unload models before use."
  (client._post "model/unload"
                :admin True)
  (setv client.model None))

(defmethod lora-load [#^ TabbyClient client #^ list loras]
  "Load LoRAs when using TabbyAPI.
  loras is a list of dicts, with 'name', 'scaling' as keys."
  (client._post "lora/load"
                :admin True
                :loras loras
                :timeout None))
    
(defmethod lora-load [#^ TabbyClient client #^ str lora * [scaling 1.0]]
  "Load a single LoRA when using TabbyAPI.
  lora is the name of the lora. Scaling defaults to 1.0."
  (lora-load client [{"name" lora "scaling" scaling}]))

(defmethod lora-unload [#^ TabbyClient client]
  "Unload LoRAs when using TabbyAPI."
  (client._post "lora/unload"
                :admin True))

(defmethod encode [#^ TabbyClient client #^ str text #** kwargs]
  "Encode a string into tokens.
  kwargs are passed to the API.
  See the API docs for options determining handling of special tokens."
  (client._post "token/encode" :text text #** kwargs))

(defmethod decode [#^ TabbyClient client #^ (of list int) tokens #** kwargs]
  "Encode a string into tokens.
  kwargs are passed to the API.
  See the API docs for options determining handling of special tokens."
  (client._post "token/decode" :tokens tokens #** kwargs))

(defn b64-encode-image [#^ str fname]
  "Encode a file (usually an image) to a base 64 string."
  (with [im (open fname "rb")]
    (-> (im.read)
        (base64.b64encode)
        (.decode "utf-8"))))

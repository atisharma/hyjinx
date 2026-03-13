## 🦑 Hyjinx 🔬

*Functions and macros useful for modern living in the [Hy](http://hylang.org) REPL.*

Compatible with Hy>=1.2.0

Hyjinx is a chaotic kitchen sink of useful things that have nothing to do with each other.
When a module stabilises, it gets spun out into its own package.


### Hylights

- A sensible, lispy `defmethod` (polymorphic type dispatch)
- ~~A Hy code beautifier and in-REPL syntax highlighting~~ -- now moved to [beautifhy](https://github.com/atisharma/beautifhy)
- ~~Pretty tracebacks with correct syntax highlighting for hy or python~~ -- now moved to [beautifhy](https://github.com/atisharma/beautifhy)
- An `inspect` module that works like Python's, but also for Hy code (see https://github.com/hylang/hy/pull/2678)
- Print/get/edit source code of a function, module etc.; e.g. `(print-source hyjinx.lib.sieve)`
- In-repl code analysis and discussion using LLMs (e.g. for writing docstrings, code review)
- In-repl syntax highlighting and latex rendering (for [sixel-capable terminals](https://www.arewesixelyet.com)) for LLM replies
- Numpy array pretty printing
- A zmq lazy pirate protocol
- A minimal ncurses class
- An async actor-model class


### Modules

- hyjinx.lib: a smorgasbord of convenience functions.
- hyjinx.source: live code inspection.
- hyjinx.inspect: code inspection.
- hyjinx.doc: peruse hy documentation.
- hyjinx.actors: the async `Actor` class and `defactor` macro
- hyjinx.screen: a convenient ncurses wrapper.
- hyjinx.result: data-oriented Result type for explicit error handling.
- hyjinx.mat: numpy pretty-printing for humans. (requires numpy, jax optional)
- hyjinx.[zmq_client, zmq_server, crypto, wire]: lazy-pirate zmq RPC architecture. (requires zmq, ecdsa, zstandard, msgpack)
- hyjinx.llm: discuss code with a Large Language Model (AI). TabbyAPI, OpenAI-compatible and Claude are supported. (requires `openai` and `anthropic` packages.)


### Error handling

`hyjinx.result` provides a lightweight Result type following Hickey's data-oriented
philosophy and Duffy's two-error-kinds model.

Results are plain dicts — observable, serialisable, composable:

```hy
(require hyjinx.result [result-> let-result match-result try-result])
(import hyjinx.result [ok err ok? err? unwrap unwrap-or])

;; Construct
(ok 42)            ; => {"ok" True, "value" 42}
(err "not-found" "File missing" {"path" fname})

;; Thread a pipeline, short-circuiting on first failure
(result-> raw-string
          parse-int          ; str -> Result[int]
          (validate-range 0 100)
          (* 2))             ; plain return is wrapped in ok automatically

;; Bind and propagate
(let-result [n (parse-int s)]
  (let-result [v (validate n)]
    (ok (* v 2))))

;; Bridge exception-throwing code at known fallible boundaries only
(try-result (json.loads text))   ; => ok(parsed) or err("JSONDecodeError", ...)
```

Three refactored functions in `hyjinx.lib` demonstrate the integration:

| Function | Change |
|---|---|
| `extract-json` | Returns `ok(dict\|list)` or `err(...)` instead of silently returning `{}` |
| `slurp-result` | Like `slurp` but returns `ok(content)` or `err("file-not-found"\|"io-error", ...)` |
| `jload-result` | Like `jload` but returns a Result, distinguishing missing file from malformed JSON |


### Install

```bash
$ pip install -U hyjinx
```

or, with optional dependencies,
```bash
$ pip install -U hyjinx[zmq]
$ pip install -U hyjinx[llm]
```

To install offline hy/hyrule documentation,
```bash
$ hy -m hyjinx.docs
```

For in-terminal sixel rendering of latex in LLM replies, make sure pdflatex, dvipng and img2sixel are installed, and that you're using a sixel-capable terminal.


### Optional dependencies

You can install with the `[zmq]` option which also installs ecdsa, [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

You can install with the `[llm]` option which also installs openai and lets you discuss code objects with ChatGPT or a locally-served LLM (via TabbyAPI or similar).

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/atisharma/hyjinx)

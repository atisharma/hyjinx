## 🦑 Hyjinx

*Functions and macros useful for modern living in the [Hy](http://hylang.org) REPL.*

Compatible with Hy 0.28.0.


### Hylights

- REPL syntax highlighting (put `(import hyjinx.source [hylight]) (setv repl-output-fn hylight)` in your .hyrc)
- pretty tracebacks with correct syntax highlighting for hy or python
- print/get/edit source code of a function, module etc.
- in-repl code analysis and discussion using LLMs
- `defmethod` (if `multimethod` is installed)
- numpy array pretty printing
- a zmq lazy pirate protocol
- a minimal ncurses class


### Modules

- hyjinx.lib: a smorgasbord of convenience functions.
- hyjinx.source: code inspection.
- hyjinx.doc: peruse hy documentation.
- hyjinx.screen: a convenient ncurses wrapper.

- hyjinx.mat: numpy pretty-printing for humans. (requires numpy, jax optional)

- hyjinx.[zmq_client, zmq_server, crypto, wire]: lazy-pirate zmq RPC architecture. (requires zmq, ecdsa, zstandard)

- hyjinx.llm: discuss code with a Large Language Model (AI). TabbyAPI, OpenAI-compatible and Claude are supported. (requires `openai` and `anthropic` packages.)


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

See hyjinx.hyrc for example usage.


### Optional dependencies

You can install with the `[zmq]` option which also installs ecdsa, [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

You can install with the `[llm]` option which also installs openai and lets you discuss code objects with ChatGPT or a locally-served LLM (via TabbyAPI or similar).

## 🦑 Hyjinx

*Functions and macros useful for modern living in the [Hy](http://hylang.org) REPL.*

Compatible with Hy 0.29.0.


### Hylights

- A Hy code beautifier and in-REPL syntax highlighting
- An `inspect` module that works like Python's, but also for Hy code
- Pretty tracebacks with correct syntax highlighting for hy or python
- Print/get/edit source code of a function, module etc. (e.g. `(print-source hyjinx.lib.sieve)`
- A sensible, lispy `defmethod`
- In-repl code analysis and discussion using LLMs (e.g. for writing docstrings)
- In-repl syntax highlighting and latex rendering (for [sixel-capable terminals](https://www.arewesixelyet.com)) for LLM replies
- Numpy array pretty printing
- A zmq lazy pirate protocol
- A minimal ncurses class


### Modules

- hyjinx.lib: a smorgasbord of convenience functions.
- hyjinx.source: live code inspection.
- hyjinx.inspect: code inspection.
- hyjinx.doc: peruse hy documentation.
- hyjinx.screen: a convenient ncurses wrapper.

- hyjinx.mat: numpy pretty-printing for humans. (requires numpy, jax optional)

- hyjinx.[zmq_client, zmq_server, crypto, wire]: lazy-pirate zmq RPC architecture. (requires zmq, ecdsa, zstandard, msgpack)

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

For syntax highlighting in the REPL, put 
```hylang
(import hyjinx.source [hylight]) (setv repl-output-fn hylight)
```
in your .hyrc.

See hyjinx.hyrc for example usage.

For in-terminal sixel rendering of latex in LLM replies, make sure pdflatex, dvipng and img2sixel are installed, and that you're using a sixel-capable terminal.


### Optional dependencies

You can install with the `[zmq]` option which also installs ecdsa, [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

You can install with the `[llm]` option which also installs openai and lets you discuss code objects with ChatGPT or a locally-served LLM (via TabbyAPI or similar).

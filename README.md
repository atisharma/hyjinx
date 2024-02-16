## 🦑 Hyjinx 🦑

Some [Hy](http://hylang.org) convenience things. Compatible with Hy 0.28.0.

#### Hylights

- REPL syntax highlighting (put `(import hyjinx.source [hylight]) (setv repl-output-fn hylight)` in your .hyrc)
- pretty tracebacks with correct syntax highlighting for hy or python
- print/get/edit source code of a function, module etc.
- `defmethod` (if `multimethod` is installed)
- numpy array pretty printing
- a zmq lazy pirate protocol
- a minimal ncurses class
- in-repl code analysis and discussion using LLMs

#### Install

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

#### Optional dependencies

You can install with the `[zmq]` option which also installs ecdsa, [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

You can install with the `[llm]` option which also installs openai and lets you discuss code objects with ChatGPT or a locally-served LLM (via TabbyAPI or similar).

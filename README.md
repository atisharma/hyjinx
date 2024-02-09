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

#### Install

```bash
$ pip install hyjinx
```

or, with optional dependencies,
```bash
$ pip install hyjinx[zmq]
```

To install offline hy/hyrule documentation,
```bash
$ hy -m hyjinx.docs
```

#### Optional dependencies

You can install with the `[zmq]` option which also installs ecdsa, [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

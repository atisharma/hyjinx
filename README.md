## Hyjinx

Some [Hy](http://hylang.org) convenience things. Compatible with Hy 0.28.0.

#### Hylights

- REPL syntax highlighting (put `(import hyjinx.source [hylight]) (setv repl-output-fn hylight)` in your .hyrc)
- print/get source code of a function, module etc.

#### Install

```bash
$ pip install hyjinx
```

#### Optional dependencies

You can install with the `[zmq]` option which also installs [pyzmq](https://pypi.org/project/pyzmq/) and [zstandard](https://pypi.org/project/zstandard/) for the zmq server/client

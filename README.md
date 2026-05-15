## 🦑 Hyjinx 🔬

*Functions and macros for modern living in the [Hy](http://hylang.org) REPL.*

Compatible with Hy ≥ 1.2.0 · Python ≥ 3.13

Hyjinx is a utility library for Hy. When a module stabilises, it gets spun out into its own package (e.g. [beautifhy](https://github.com/atisharma/beautifhy)).


### Modules

| Module | Description | Dependencies |
|--------|-------------|--------------|
| `lib` | String/file/OS utilities, JSON I/O, hashing | toolz |
| `result` | Result type (`ok`/`err`) with threading macros | — |
| `macros` | `defmethod`, `defstruct`, `when-let`, `lmap`, etc. | toolz |
| `actors` | Async actor model with `defactor` macro | — |
| `api` | Static API surface inspection (no imports, no side effects) | — |
| `source` | Code inspection, display, and editing | pygments, beautifhy |
| `hjx_inspect` | Hy-aware `inspect` module | multimethod |
| `crypto` | ECDSA signing and password hashing | ecdsa |
| `mat` | NumPy pretty-printing for humans | numpy, colorist |
| `llm` | Discuss code with LLMs (OpenAI, Anthropic, TabbyAPI) | openai, anthropic |
| `wire` | ZMQ message framing | zmq, msgpack, zstandard |
| `zmq_client` | Lazy Pirate ZMQ client | zmq |
| `zmq_server` | Lazy Pirate ZMQ server | zmq |
| `screen` | ncurses wrapper | — |
| `docs` | Offline Hy/Hyrule/Toolz documentation | — |
| `hyrc` | REPL startup configuration | — |
| `mail` | Email sending | — |
| `cli` | Command-line interface | click, pygments |


### CLI

```bash
$ hyjinx where hyjinx.macros.defmethod
/path/to/hyjinx/macros.hy:94

$ hyjinx where json.dumps --json
{"line": 184, "module": "json", "file": "...", "language": "python", "extension": "py"}

$ hyjinx dir hyjinx.result
hyjinx/result.hy  [14 definitions]
  defn ok(value)
  defn err(error-type, message, context)
  defn as-result(x)
  mac  result->(expr, forms)
  mac  result->>(expr, forms)
  mac  let-result(bindings, body)
  mac  match-result(result, ok-sym, ok-body, err-sym, err-body)
  mac  try-result(body)
  defn raise-if-err(result)
  defn unwrap(result)
  defn unwrap-or(result, default)
  defn map-ok(f, result)
  defn map-err(f, result)
  defn collect-results(results)

$ hyjinx dir hyjinx.lib --kind defn --no-params
hyjinx/lib.hy  [60 definitions]
  defn mreload
  defn sync-await
  defn compose
  ...

$ hyjinx source hyjinx.api.format-surface --no-highlight
(defn format-surface [defs * [show-params True] [show-line False]]
  ...)

$ hyjinx doc hyjinx.api.api-surface
Return a list of definition dicts for a source file.
Each dict has keys: kind, name, params, line.
No imports are performed — purely static analysis.
```

The `dir` command uses **static analysis** — it parses source files without importing them, so it never triggers side effects (DB connections, GPU init, etc.).


### Install

```bash
pip install hyjinx
```

Optional dependencies:

```bash
pip install hyjinx[zmq]   # ecdsa, pyzmq, zstandard, msgpack
pip install hyjinx[llm]   # openai, anthropic
pip install hyjinx[dev]   # pytest + all optional dependencies
```

For offline Hy/Hyrule/Toolz documentation:

```bash
hy -m hyjinx.docs
```

For in-terminal sixel rendering of LaTeX in LLM replies, install `pdflatex`, `dvipng`, and `img2sixel`, and use a sixel-capable terminal.


### Hylights

- **`defmethod`** — polymorphic type dispatch via `multimethod`
- **Result type** — `ok`/`err` with `result->`, `let-result`, `match-result`, `try-result`
- **`defstruct`** — frozen dict-like records
- **Static API inspection** — `api-surface` parses Hy/Python source without importing
- **Hy-aware `inspect`** — `getsource`, `getsourcefile` that handle macros and multimethods
- **LLM integration** — discuss code with ChatGPT, Claude, or local models
- **ZMQ lazy pirate** — reliable RPC client/server with automatic retries

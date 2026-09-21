"""
🦑 - Functions and macros for modern living in the Hy REPL.

▌      ▖▗       
▛▀▖▌ ▌▗▖▄ ▛▀▖▚▗▘
▌ ▌▚▄▌ ▌▐ ▌ ▌▗▚ 
▘ ▘▗▄▘▄▘▀▘▘ ▘▘ ▘

hyjinx.lib          - a smorgasbord of convenience functions.
hyjinx.source       - code inspection.
hyjinx.hjx_inspect  - code inspection, similar to python's inspect.
hyjinx.doc          - peruse hy documentation.
hyjinx.actors       - a convenient asyncio agent model
hyjinx.screen       - a convenient ncurses wrapper.
hyjinx.result       - Result type for explicit error handling.
hyjinx.debug        - Hy-aware pdb debugger with macro support.

(depends on numpy and/or jax)
hyjinx.mat          - numpy pretty-printing for humans.

(depends on zmq, ecdsa, zstandard)
hyjinx.[zmq_client, zmq_server, crypto, wire] - lazy-pirate zmq RPC architecture.

(depends on openai and/or anthropic)
hyjinx.llm          - discuss code with a Large Language Model (AI). Anthropic, TabbyAPI and OpenAI-compatible APIs are supported.

"""

import importlib
import importlib.util

import hy

from toolz.itertoolz import rest

from toolz import (
    first,
    second,
    last,
    take,
    drop,
    partition,
    identity
    )

from functools import (
    partial,
    reduce,
    cache
    )

from itertools import (
    accumulate,
    batched,
    chain,
    pairwise,
    repeat
    )


# * Lazy submodule exports (PEP 562)
# ----------------------------------------------------
# Submodule exports load on first access instead of at import.  The eager
# import block that used to live here made `import hyjinx` pull in every
# submodule -- hyjinx.lib -> hyrule, hyjinx.source -> beautifhy, numpy via
# hyjinx.mat -- for ~4.5s, even for a caller that wanted only a toolz
# re-export or a macro.  The names below are grouped by owning submodule;
# __getattr__ imports the owner on first use and caches the value in the
# module globals.  The name lists are the exact export surface of the old
# block, mangled names included.
#
# Deferring hyjinx.debug also defers its sys.breakpointhook monkey-patch
# (debug.py installs set_trace at import), so breakpoint() only routes to
# HyPdb once something first touches hyjinx.debug.  Nothing in the trading
# suite imports it, and a utility import should not hijack breakpoint()
# globally; recorded here so the deferral is a decision, not an accident.

_LAZY_EXPORTS = {
    # hyjinx.lib — convenience functions
    "lib": (
        # modules
        "mreload",
        # async
        "sync_await",
        "coroutine",
        # functions
        "named_partial",
        "compose",
        # time
        "timestamp",
        "days_ago",
        "yesterday",
        "tomorrow",
        "now",
        # OS
        "mkdir",
        "hyx_Xexclamation_markX",
        "pwd",
        "cd",
        "ls",
        "shell",
        "username",
        # strings
        "grepp",
        "get_numeric",
        "sstrip",
        "similar",
        "decimal_align",
        "unicode_search",
        "camel_to_underscore",
        "is_url",
        # numeric
        "as_float",
        "isnumeric",
        "sign",
        "round_to",
        "hyx_posXquestion_markX",
        "hyx_negXquestion_markX",
        "hyx_zeroXquestion_markX",
        "hyx_numberXquestion_markX",
        "dice",
        "prod",
        # output
        "pp",
        "hash_color",
        "progress",
        # collections
        "sieve",
        "shift",
        "get_in",
        "group",
        # config / files
        "config",
        "slurp",
        "slurp_result",
        "spit",
        "template",
        "pload",
        "psave",
        "extract_json",
        "jload",
        "jload_result",
        "jsave",
        "jappend",
        "jsonl_append",
        "jprint",
        "filetype",
        "filenames",
        # ids / hashing
        "hash_id",
        "short_id",
        "db_url",
    ),
    # hyjinx.source — code inspection
    "source": (
        "edit",
        "get_source_details",
        "print_source",
        "interact",
        "inject_exception_hook",
    ),
    # hyjinx.docs — documentation browser
    "docs": (
        "install",
        "doc_hy",
        "doc_hyrule",
        "doc_toolz",
        "doc",
    ),
    # hyjinx.result — explicit error handling
    "result": (
        "ok",
        "err",
        "as_result",
        "unwrap",
        "unwrap_or",
        "map_ok",
        "map_err",
        "collect_results",
    ),
    # hyjinx.debug — Hy-aware debugger
    "debug": (
        "HyPdb",
        "set_trace",
    ),
    # hyjinx.call_graph — static call-graph extraction
    "call_graph": (
        "call_graph",
        "callers",
        "callees",
        "transitive_callers",
        "transitive_callees",
        "dead_code",
        "reachable",
        "call_path",
    ),
    # hyjinx.mat — numpy pretty-printing (optional: needs numpy)
    "mat": (
        "last_col",
        "drop_first_rows",
        "drop_first_cols",
        "drop_last_cols",
        "take_last_rows",
        "take_last_cols",
        "ppa",
        "describe",
    ),
}

_NAME_TO_MODULE = {
    name: module
    for module, names in _LAZY_EXPORTS.items()
    for name in names
}

# The old eager block guarded hyjinx.mat with try/except ModuleNotFoundError
# (numpy may be absent), so the mat names were not exported then.  Mirror that
# when building __all__: a star import must not fail on the missing optional
# submodule.  hyjinx.mat imports numpy at module level, so numpy's presence is
# the test.
try:
    _HAS_MAT = importlib.util.find_spec("numpy") is not None
except (ImportError, ValueError):
    _HAS_MAT = False

_EAGER_NAMES = [
    "rest",
    "first",
    "second",
    "last",
    "take",
    "drop",
    "partition",
    "identity",
    "partial",
    "reduce",
    "cache",
    "accumulate",
    "batched",
    "chain",
    "pairwise",
    "repeat",
]

__all__ = _EAGER_NAMES + [
    name for name, module in _NAME_TO_MODULE.items()
    if module != "mat" or _HAS_MAT
]


def _import_lazy(module_name, attribute):
    """Import a lazy submodule, degrading the optional ``mat`` to a missing
    attribute.

    Only hyjinx.mat is optional (it imports numpy at module level and the old
    eager block guarded it with try/except).  Any other ModuleNotFoundError is
    a real missing dependency and must propagate, not masquerade as a missing
    attribute.
    """
    try:
        return importlib.import_module(f".{module_name}", __name__)
    except ModuleNotFoundError as exc:
        if module_name != "mat":
            raise
        raise AttributeError(
            f"module {__name__!r} has no attribute {attribute!r}"
        ) from exc


def __getattr__(name):
    """Import a submodule export on first access (PEP 562).

    Preserves the package surface (``hyjinx.jload``, ``from hyjinx import *``,
    bare submodule access) without importing a submodule the caller did not
    ask for.
    """
    module_name = _NAME_TO_MODULE.get(name)
    if module_name is not None:
        value = getattr(_import_lazy(module_name, name), name)
        globals()[name] = value
        return value
    if name in _LAZY_EXPORTS:
        # Bare submodule access (hyjinx.lib, hyjinx.docs, ...).  ``call_graph``
        # is also an exported function and was handled above, matching the old
        # ``from hyjinx.call_graph import call_graph`` shadowing of the module.
        module = _import_lazy(name, name)
        globals()[name] = module
        return module
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


def __dir__():
    "Full surface for introspection, lazy names included."
    return sorted(set(globals()) | set(_NAME_TO_MODULE) | set(_LAZY_EXPORTS))


# require all the macros
hy.macros.require('hyjinx.macros', None, assignments='ALL', prefix='')

# set the package version
# the major.minor version simply match the assumed Hy version
__version__ = "1.3.0"
__version_info__ = __version__.split(".")

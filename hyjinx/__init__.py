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

(depends on numpy and/or jax)
hyjinx.mat          - numpy pretty-printing for humans.

(depends on zmq, ecdsa, zstandard)
hyjinx.[zmq_client, zmq_server, crypto, wire] - lazy-pirate zmq RPC architecture.

(depends on openai and/or anthropic)
hyjinx.llm          - discuss code with a Large Language Model (AI). Anthropic, TabbyAPI and OpenAI-compatible APIs are supported.

"""

import hy

from hyjinx.lib import *
from hyjinx.source import *
from hyjinx.docs import *

# Result type — explicit error handling for expected failures
from hyjinx.result import (
    ok,
    err,
    hyx_okXquestion_markX as is_ok,
    hyx_errXquestion_markX as is_err,
    hyx_resultXquestion_markX as is_result,
    as_result,
    unwrap,
    unwrap_or,
    map_ok,
    map_err,
    collect_results,
)

# numpy may not be installed
try:
    from hyjinx.mat import *
except ModuleNotFoundError:
    pass

# require all the macros
hy.macros.require('hyjinx.macros', None, assignments = 'ALL', prefix = '')

# set the package version
# the major.minor version simply match the assumed Hy version
__version__ = "1.2.1"
__version_info__ = __version__.split(".")

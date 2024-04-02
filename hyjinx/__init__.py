"""
🦑 - Functions and macros for modern living in the Hy REPL.

hyjinx.lib - a smorgasbord of convenience functions.
hyjinx.source - code inspection.
hyjinx.doc - peruse hy documentation.
hyjinx.screen - a convenient ncurses wrapper.

(depends on numpy and/or jax)
hyjinx.mat - numpy pretty-printing for humans.

(depends on zmq, ecdsa, zstandard)
hyjinx.[zmq_client, zmq_server, crypto, wire] - lazy-pirate zmq RPC architecture.

(depends on openai)
hyjinx.llm - discuss code with a Large Language Model (AI). TabbyAPI and OpenAI-compatible are supported.

"""

import hy

from hyjinx.lib import *
from hyjinx.source import *
from hyjinx.docs import *
from hyjinx.screen import Screen

# numpy may not be installed
try:
    from hyjinx.mat import *
except ModuleNotFoundError:
    pass

# require all the macros
hy.macros.require('hyjinx.macros', None, assignments = 'ALL', prefix = '')

# set the package version
__version__ = "0.28.22"
__version_info__ = __version__.split(".")

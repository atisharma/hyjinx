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

(depends on openai and optionally anthropic)
hyjinx.llm - discuss code with a Large Language Model (AI). TabbyAPI and OpenAI-compatible are supported.

"""

import hy

from hyjinx import inspect

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
__version__ = "0.28.27"
__version_info__ = __version__.split(".")


def __cli_grind_files():
    """Pretty-print hy files from the shell."""
    # The first arg is script name, ignore it.
    import sys
    import hyjinx.beautify
    for fname in sys.argv[1:]:
        if fname.endswith(".hy"):
            hyjinx.beautify.grind_file(fname)
            print()

def __cli_hylight_files():
    """Syntax highlight hy or python files from the shell."""
    # The first arg is script name, ignore it.
    import sys
    import hyjinx.source
    for fname in sys.argv[1:]:
        if fname.endswith(".hy"):
            lexer = hyjinx.source.get_lexer_by_name("hylang")
            formatter = hyjinx.source.TerminalFormatter(linenos=False, bg="light", stripall=True)
            code = slurp(fname)
            print()
            print(highlight(code, lexer, formatter))
            print()
        elif fname.endswith(".py"):
            lexer = hyjinx.source.get_lexer_by_name("python")
            formatter = hyjinx.source.TerminalFormatter(linenos=False, bg="light", stripall=True)
            code = slurp(fname)
            print()
            print(highlight(code, lexer, formatter))
            print()

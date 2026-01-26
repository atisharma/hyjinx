"""
Get useful information from live Hy or Python objects.

This module extend's Hy compatibility with Hy's hy_inspect module to to cover
functools' `partial` objects and `multimethod`s. Within Hy, Hy macros are
supported via Hy's `get_macro` macro, for example `(inspect.getsource
(get-macro get-macro))`.

Other functions are imported from CPython's `inspect` module. See that module's
documentation for descriptions as they relate to Python.
"""

import importlib
import inspect
import linecache
import os
import sys
from functools import partial
from inspect import *

from multimethod import multimethod


def ismultimethod(object):
    return isinstance(object, multimethod)


def ispartial(object):
    return isinstance(object, partial)


def getfile(object):
    """Work out which source or compiled file an object was defined in."""

    if ismultimethod(object):
        if hasattr(object, "__module__") and object.__module__ is not None:
            module = sys.modules.get(object.__module__)
            if module and getattr(module, "__file__", None):
                return module.__file__
        if hasattr(object, "__module__") and object.__module__ == "__main__":
            raise OSError("source code not available")
        else:
            raise TypeError("{!r} is an unhandled multimethod".format(object))

    elif ispartial(object):
        # For partials, try to get file from the wrapped function
        if hasattr(object, "func"):
            try:
                return getfile(object.func)
            except (TypeError, OSError):
                pass
        # Fallback to module approach
        if hasattr(object, "__module__") and object.__module__ is not None:
            module = sys.modules.get(object.__module__)
            if module and getattr(module, "__file__", None):
                return module.__file__
        if hasattr(object, "__module__") and object.__module__ == "__main__":
            raise OSError("source code not available")
        raise TypeError("{!r} is an unhandled partial".format(object))

    else:
        return inspect.getfile(object)


def findsource(object):
    """Return the entire source file and starting line number for an object.

    First looks for Hy source, otherwise defers to the original
    `inspect.findsource`. The argument may be a module, class, method,
    function, traceback, frame, or code object.  The source code is returned as
    a list of all the lines in the file and the line number indexes a line in
    that list.

    An OSError is raised if the source code cannot be retrieved.
    """

    # Identify Hy objects from file extension
    if ispartial(object):
        # A partial has a func atrribute and carries its args and
        # keywords with it.
        return findsource(object.func)

    elif getfile(object).endswith(".hy") and not inspect.isframe(object):
        if ismultimethod(object):
            # A multimethod's values are functions.
            # So, this is a tricky one, because methods can be defined
            # in more than one place.
            # We assume they're in the same file and return the location
            # of the first one.
            methods = list(object.values())
            # Try to find the first method with accessible source
            for method in methods:
                try:
                    return findsource(method)
                except (OSError, TypeError):
                    continue

            # If no method has source, raise
            raise OSError(
                "no registered multimethod implementation has accessible source"
            )

    else:
        return inspect.findsource(object)


def getsourcelines(object):
    """Return a list of source lines and starting line number for a Hy or python object.

    First checks for Hy source, otherwise defers to the original `inspect.getsourcelines`.

    The argument may be a module, class, method, function, traceback, frame, or
    code object. The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found. An OSError is
    raised if the source code cannot be retrieved.
    """
    if ispartial(object):
        object = object.func
    return inspect.getsourcelines(object)

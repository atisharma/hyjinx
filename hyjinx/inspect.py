"""
Get useful information from live Hy or Python objects.

This module provides Hy compatibility with Cpython's inspect module.
Its functionality has been extended to cover functools' `partial`
objects and `multimethod`s.

The `findsource` and `getsourcelines` functions involve applying the
Hy reader and compiling Hy code, which executes any code defined in
reader macros, so consider this fact before using them. Other
functions are imported from CPython's `inspect` module. See below for
descriptions as they relate to Python.

The module encapsulates the interface provided by the internal special
attributes (co_*, im_*, tb_*, etc.) in a friendlier fashion.
It also provides some help for examining source code and class layout.

Here are some of the useful functions provided by this module:

    ismodule(), isclass(), ismethod(), isfunction(), isgeneratorfunction(),
        isgenerator(), istraceback(), isframe(), iscode(), isbuiltin(),
        isroutine() - check object types
    getmembers() - get members of an object that satisfy a given condition

    getfile(), getsourcefile(), getsource() - find an object's source code
    getdoc(), getcomments() - get documentation on an object
    getmodule() - determine the module that an object came from
    getclasstree() - arrange classes so as to represent their hierarchy

    getargvalues(), getcallargs() - get info about function arguments
    getfullargspec() - same, with support for Python 3 features
    formatargvalues() - format an argument spec
    getouterframes(), getinnerframes() - get info about frames
    currentframe() - get the current stack frame
    stack(), trace() - get info about frames on the stack or in a traceback

    signature() - get a Signature object for the callable

    get_annotations() - safely compute an object's annotations
"""

import os
import sys
import importlib
import linecache
import inspect

from inspect import *
from itertools import chain
from functools import partial

from hy import repr
from hy.reader import read_many
from hy.compiler import hy_compile
from hy.models import Expression, Lazy

from multimethod import multimethod


def ismultimethod(object):
    return isinstance(object, multimethod)
    
def ispartial(object):
    return isinstance(object, partial)
    
def isExpression(object):
    return isinstance(object, Expression)
    
def isLazy(object):
    return isinstance(object, Lazy)
    
def getfile(object):
    """Work out which source or compiled file an object was defined in."""
    if isLazy(object) and hasattr(object, 'filename'):
        return object.filename
    if isExpression(object) and hasattr(object, 'filename'):
        return object.filename
    if ismultimethod(object) and hasattr(object, '__module__'):
        module = sys.modules.get(object.__module__)
        if getattr(module, '__file__', None):
            return module.__file__
        elif object.__module__ == '__main__':
            raise OSError('source code not available')
        else:
            raise TypeError('{!r} is an unhandled multimethod'.format(object))
    elif ispartial(object) and hasattr(object, '__module__'):
        module = sys.modules.get(object.__module__)
        if getattr(module, '__file__', None):
            return module.__file__
        elif object.__module__ == '__main__':
            raise OSError('source code not available')
        else:
            raise TypeError('{!r} is an unhandled partial'.format(object))
    elif isExpression(object):
        raise OSError('source code not available')
    elif any([ismodule(object), isclass(object), ismethod(object), isfunction(object), istraceback(object), isframe(object), iscode(object)]):
        return inspect.getfile(object)
    else:
        raise TypeError('module, class, method, multimethod, partial, function, '
                        'traceback, frame, code, or Hy expression object was '
                        'expected, got {}'.format(type(object).__name__))

def getsourcefile(object):
    """Return the filename that can be used to locate an object's source.
    Return None if no way can be identified to get the source."""
    filename = getfile(object)
    all_bytecode_suffixes = importlib.machinery.DEBUG_BYTECODE_SUFFIXES[:]
    all_bytecode_suffixes += importlib.machinery.OPTIMIZED_BYTECODE_SUFFIXES[:]
    if any(filename.endswith(s) for s in all_bytecode_suffixes):
        filename = (os.path.splitext(filename)[0] + importlib.machinery.SOURCE_SUFFIXES[0])
    elif any(filename.endswith(s) for s in importlib.machinery.EXTENSION_SUFFIXES):
        return None
    # return a filename found in the linecache even if it doesn't exist on disk
    if filename in linecache.cache:
        return filename
    if os.path.exists(filename):
        return filename
    # only return a non-existent filename if the module has a PEP 302 loader
    module = getmodule(object, filename)
    if getattr(module, '__loader__', None) is not None:
        return filename
    elif getattr(getattr(module, "__spec__", None), "loader", None) is not None:
        return filename
    
def findsource(object):
    """Return the entire source file and starting line number for an object.

    First looks for Hy source, otherwise defers to the original `inspect.findsource`.
    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of all the lines
    in the file and the line number indexes a line in that list.  An OSError
    is raised if the source code cannot be retrieved."""
    file = getsourcefile(object)
    # Identify Hy objects from file extension
    if ispartial(object):
        # A partial has a func atrribute and carries its args and
        # keywords with it.
        return findsource(object.func)
    elif isExpression(object) or isLazy(object):
        lnum = object.start_line
        lines = linecache.getlines(file)
        return (lines, lnum)
    elif getfile(object).endswith('.hy') and not inspect.isframe(object):
        module = getmodule(object, file)
        # list of source code lines
        lines = linecache.getlines(file, module.__dict__) if module else linecache.getlines(file)
        if inspect.ismodule(object):
            return (lines, 0)
        # some objects already have the information we need
        elif hasattr(object, "__code__") and hasattr(object.__code__, "co_firstlineno"):
            lnum = object.__code__.co_firstlineno - 1
            return (lines, lnum)
        # Hy classes do not, so read the first form from the location
        # Calling read_many / compile can execute code.
        # Cache pst on first compile?
        elif isclass(object):
            qualname = object.__qualname__
            source = ''.join(lines)
            hst = read_many(source, filename=file, skip_shebang=True)
            pst = hy_compile(hst, module, filename=file, source=source)
            class_finder = inspect._ClassFinder(qualname)
            try:
                return class_finder.visit(pst)
            except (inspect.ClassFoundException,) as e:
                return (lines, e.args[0])
        elif ismultimethod(object):
            # A multimethod's values are functions.
            # So, this is a tricky one, because methods can be defined
            # in more than one place.
            # We assume they're in the same file and return the location
            # of the first one.
            methods = list(object.values())
            return findsource(methods[0])
    else:
        # Non-Hy object
        return inspect.findsource(object)

def getcomments(object):
    """
    Get comments relating to an object's source code.

    First checks for Hy source, otherwise defers to the original `inspect.getcomments`.
    Returns None when the source can't be found.
    """
    if getfile(object).endswith('.hy'):
        # Roughly follows the logic of inspect.getcomments, but for Hy comments
        try:
            [lines, lnum] = findsource(object)
            comments = []
            if inspect.ismodule(object) or isExpression(object) or isLazy(object):
                # Remove shebang.
                start = 1 if lines and lines[0][:2] == '#!' else 0
                # Remove preceding empty lines and textless comments.
                while start < len(lines) and set(lines[start].replace(' ', '')) == (';',):
                    start += 1
                if start < len(lines) and lines[start].lstrip().startswith(';'):
                    end = start
                    while end < len(lines) and lines[end].lstrip().startswith(';'):
                        comments.append(lines[end].expandtabs())
                        end += 1
                    return ''.join(comments)
                else:
                    return None
            elif ispartial(object):
                return getcomments(object.func)
            # Look for a comment block preceding the object
            elif lnum > 0:
                # Look for comments above the object and work up.
                indent = inspect.indentsize(lines[lnum])
                end = lnum - 1
                while end >= 0 and lines[end].lstrip().startswith(';'):
                    comments = [lines[end].expandtabs().lstrip()] + comments
                    end -= 1
                return ''.join(comments)
            else:
                return None
        # Return None when the source can't be found.
        except (OSError, TypeError) as e:
            return None
    else:
        # Non-Hy object
        return inspect.getcomments(object)

def getsourcelines(object):
    """Return a list of source lines and starting line number for a Hy or python object.

    First checks for Hy source, otherwise defers to the original `inspect.getsourcelines`.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found.  An OSError is
    raised if the source code cannot be retrieved."""
    object = inspect.unwrap(object)
    lines, lnum = findsource(object)

    if inspect.istraceback(object):
        object = object.tb_frame
    elif ispartial(object):
        object = object.func

    # For module or frame that corresponds to module, return all source lines.
    if (inspect.ismodule(object) or (inspect.isframe(object) and object.f_code.co_name == "<module>")):
        return lines, 0
    # Everything but Hy classes already works with inspect. 
    # The inspect.getblock function relies on inspect.BlockFinder which
    # assumes python tokenization. So deal with this as a special case.
    elif getfile(object).endswith('.hy'):
        # Read the first form that was found using inspect.findsource.
        form = next(read_many(''.join(lines[lnum:]), filename=getfile(object), skip_shebang=True))
        # Pretty-print it so it looks like the source, dropping the '
        source = repr(form)[1:]
        return source, lnum
    else:
        # Non-Hy object
        return inspect.getblock(lines[lnum:]), lnum + 1

def getsource(object):
    """Return the text of the source code for an object.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a single string.  An
    OSError is raised if the source code cannot be retrieved."""
    lines, lnum = getsourcelines(object)
    return ''.join(lines)


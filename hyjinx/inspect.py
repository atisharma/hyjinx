"""
Get useful information from live Hy or Python objects.

This module provides Hy compatibility with Cpython's inspect module. Its
functionality has been extended to cover functools' `partial` objects and
`multimethod`s. Within Hy, Hy macros are supported via Hy's `get_macro` macro,
for example `(inspect.getsource (get-macro get-macro))`.

Other functions are imported from CPython's `inspect` module. See below for
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
from functools import partial

from hy import repr
from hy.reader import read, read_many
from hy.models import Expression, Lazy

from beautifhy.reader import HyReader, HySafeReader

from multimethod import multimethod

# It would be better to subclass HyReader to a HySafeReader,
# to avoid this dependency
#from pygments.lexers import HyLexer
#from pygments.token import Token


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

    if ismultimethod(object):
        if hasattr(object, '__module__') and object.__module__ is not None:
            module = sys.modules.get(object.__module__)
            if module and getattr(module, '__file__', None):
                return module.__file__
        if hasattr(object, '__module__') and object.__module__ == '__main__':
            raise OSError('source code not available')
        else:
            raise TypeError('{!r} is an unhandled multimethod'.format(object))

    elif ispartial(object):
        # For partials, try to get file from the wrapped function
        if hasattr(object, 'func'):
            try:
                return getfile(object.func)
            except (TypeError, OSError):
                pass
        # Fallback to module approach
        if hasattr(object, '__module__') and object.__module__ is not None:
            module = sys.modules.get(object.__module__)
            if module and getattr(module, '__file__', None):
                return module.__file__
        if hasattr(object, '__module__') and object.__module__ == '__main__':
            raise OSError('source code not available')
        raise TypeError('{!r} is an unhandled partial'.format(object))

    elif isExpression(object):
        raise OSError('source code not available')

    elif any([ismodule(object),
              isclass(object),
              ismethod(object),
              isfunction(object),
              istraceback(object),
              isframe(object),
              iscode(object)]):
        return inspect.getfile(object)

    elif hasattr(object, '__class__'):
        # Avoid infinite recursion by ensuring it's not the same object
        if object.__class__ is type(object):
            raise TypeError('cannot determine file for object of type {}'.format(type(object).__name__))
        return getfile(object.__class__)

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

    First looks for Hy source, otherwise defers to the original
    `inspect.findsource`. The argument may be a module, class, method,
    function, traceback, frame, or code object.  The source code is returned as
    a list of all the lines in the file and the line number indexes a line in
    that list.

    An OSError is raised if the source code cannot be retrieved.
    """
    file = getsourcefile(object)

    # Identify Hy objects from file extension
    if ispartial(object):
        # A partial has a func atrribute and carries its args and
        # keywords with it.
        return findsource(object.func)

    elif isExpression(object) or isLazy(object):
        if not hasattr(object, 'start_line') or object.start_line is None:
            raise OSError('source code not available')
        lnum = object.start_line
        lines = linecache.getlines(file)
        return (lines, lnum)

    elif getfile(object).endswith('.hy') and not inspect.isframe(object):
        module = getmodule(object, file)
        # List of source code lines.
        lines = linecache.getlines(file, module.__dict__) if module else linecache.getlines(file)
        if inspect.ismodule(object):
            return (lines, 0)
        # Some objects already have the information we need.
        elif hasattr(object, "__code__") and hasattr(object.__code__, "co_firstlineno"):
            lnum = object.__code__.co_firstlineno - 1
            return (lines, lnum)
        elif isclass(object):
            try:
                # _ClassFinder not in python 3.13, but findsource works directly
                if sys.version_info <= (3, 12):
                    qualname = object.__qualname__
                    source = ''.join(lines)
                    hst = read_many(source, filename=file, skip_shebang=True, reader=HySafeReader())
                    pst = hy_compile(hst, module, filename=file, source=source)
                    class_finder = inspect._ClassFinder(qualname)
                    return class_finder.visit(pst)
                else:
                    return inspect.findsource(object)
            except (inspect.ClassFoundException,) as e:
                return (lines, e.args[0])
        elif ismultimethod(object):
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
            raise OSError('no registered multimethod implementation has accessible source')
    else:
        # Non-Hy object
        return inspect.findsource(object)

def getcomments(object):
    """
    Get comments relating to an object's source code.

    First checks for Hy source, otherwise defers to the original `inspect.getcomments`.
    Returns None when the source can't be found.
    """
    try:
        file = getfile(object)
    except (OSError, TypeError):
        return None

    if getfile(object).endswith('.hy'):
        # Roughly follows the logic of inspect.getcomments, but for Hy comments
        try:
            [lines, lnum] = findsource(object)

            if not lines:
                return None

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
            elif lnum > 0 and lnum < len(lines):
                # Look for comments above the object and work up.
                indent = inspect.indentsize(lines[lnum])
                end = lnum - 1
                while end >= 0 and lines[end].lstrip().startswith(';'):
                    comments = [lines[end].expandtabs().lstrip()] + comments
                    end -= 1
                return ''.join(comments) if comments else None
            else:
                return None
        # Return None when the source can't be found.
        except (OSError, TypeError, IndexError, AttributeError) as e:
            return None
    else:
        # Non-Hy object
        return inspect.getcomments(object)

def getsource(object):
    """Return the text of the source code for an object.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a single string.  An
    OSError is raised if the source code cannot be retrieved."""
    lines, lnum = getsourcelines(object)
    return ''.join(lines)

def hy_getblock(lines):
    """Extract the lines of code corresponding to the first Hy form from the given list of lines."""
    # Read the first form and use its attributes
    form = read(''.join(lines), reader=HySafeReader())
    return lines[:form.end_line]


def getsourcelines(object):
    """Return a list of source lines and starting line number for a Hy or python object.

    First checks for Hy source, otherwise defers to the original `inspect.getsourcelines`.

    The argument may be a module, class, method, function, traceback, frame, or
    code object. The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found. An OSError is
    raised if the source code cannot be retrieved.

    This function involves applying a 'safe' subclassed Hy reader, which does
    not execute any code defined in user-defined reader macros. This avoids
    arbitrary code execution when inspecting untrusted objects.
    """
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
    # assumes python tokenization.
    # So deal with this as a special case.
    elif getfile(object).endswith('.hy'):
        return hy_getblock(lines[lnum:]), lnum + 1
    else:
        # Non-Hy object
        return inspect.getblock(lines[lnum:]), lnum + 1

def lexer_getsourcelines(object):
    """Return a list of source lines and starting line number for a Hy or python object.

    Operates on Hy source, otherwise defers to python's `inspect.getsourcelines`.

    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found.  An OSError is
    raised if the source code cannot be retrieved.

    This function uses lexical analysis only and does not execute reader macros
    or compile code. It may not work correctly in cases where reader macros
    alter the syntax in some specific ways. If you need accurate identification
    of source lines that includes such reader macros, use
    `unsafe_getsourcelines`.
    """
    object = inspect.unwrap(object)
    lines, lnum = findsource(object)

    if inspect.istraceback(object):
        object = object.tb_frame
    elif ispartial(object):
        object = object.func

    # For module or frame that corresponds to module, return all source lines.
    if (inspect.ismodule(object) or (inspect.isframe(object) and object.f_code.co_name == "<module>")):
        return lines, 0
    # For Hy objects, extract source by finding the complete form
    elif getfile(object).endswith('.hy'):
        # Use lexer to find where the first form ends, but preserve original lines
        source_text = ''.join(lines[lnum:])
        
        lexer = HyLexer()
        tokens = list(lexer.get_tokens(source_text))
        
        # Track parenthesis depth and character position
        depth = 0
        char_pos = 0
        form_end_pos = 0
        started = False
        
        for token_type, value in tokens:
            if token_type == Token.Punctuation:
                if value in '([{':
                    depth += 1
                    started = True
                elif value in ')]}':
                    depth -= 1
            
            char_pos += len(value)
            
            # Found complete form
            if started and depth == 0:
                form_end_pos = char_pos
                break
        
        # Now extract that many characters from the original lines
        if form_end_pos > 0:
            chars_counted = 0
            result_lines = []
            
            for line in lines[lnum:]:
                if chars_counted + len(line) <= form_end_pos:
                    result_lines.append(line)
                    chars_counted += len(line)
                else:
                    # Partial line - include up to form_end_pos
                    remaining = form_end_pos - chars_counted
                    result_lines.append(line[:remaining])
                    break
                    
                if chars_counted >= form_end_pos:
                    break
            
            return result_lines, lnum + 1
        else:
            # Fallback: couldn't parse, return first line
            return [lines[lnum]], lnum + 1
    else:
        # Non-Hy object
        return inspect.getblock(lines[lnum:]), lnum + 1

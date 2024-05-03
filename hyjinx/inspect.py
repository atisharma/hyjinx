"""
Get useful information from live Hy or Python objects.

This module provides Hy compatibility with Cpython's inspect module.

The `findsource` and `getsourcelines` involve applying the Hy reader
and compiling Hy code, which executes any code defined in reader macros,
so do not use with untrusted Hy code.

It encapsulates the interface provided by the internal special
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

import sys
import inspect
from inspect import *

# Don't take Hy 0.28-dev patch for getsourcefile
import hy._compat
if hasattr(hy._compat, "true_getsourcefile"):
    __hy_compat_getsourcefile = inspect.getsourcefile
    getsourcefile = hy._compat.true_getsourcefile
    
# cache methods that will be patched
inspect_findsource = inspect.findsource
inspect_getcomments = inspect.getcomments
inspect_getsourcelines = inspect.getsourcelines
inspect_getsource = inspect.getsource

def ismultimethod(object):
    from multimethod import multimethod
    return isinstance(object, multimethod)
    
def getfile(object):
    """Work out which source or compiled file an object was defined in."""
    if ismultimethod(object):
        if hasattr(object, '__module__'):
            module = sys.modules.get(object.__module__)
            if getattr(module, '__file__', None):
                return module.__file__
            if object.__module__ == '__main__':
                raise OSError('source code not available')
        raise TypeError('{!r} is an unhandled multimethod'.format(object))
    else:
        return getfile(object)

def findsource(object):
    """Return the entire source file and starting line number for an object.

    First looks for Hy source, otherwise defers to the original `inspect.findsource`.
    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of all the lines
    in the file and the line number indexes a line in that list.  An OSError
    is raised if the source code cannot be retrieved."""
    import linecache
    from hy.reader import read_many
    from hy.compiler import hy_compile
    file = getsourcefile(object)

    if inspect.getfile(object).endswith('.hy') and not inspect.isframe(object):
        module = getmodule(object, file)
        # list of source code lines
        lines = linecache.getlines(file, module.__dict__) if module else linecache.getlines(file)
        # some objects already have the information we need
        if hasattr(object, "__code__") and hasattr(object.__code__, "co_firstlineno"):
            lnum = object.__code__.co_firstlineno - 1
            return (lines, lnum)
        # Hy classes do not, so read the first form from the location
        elif isclass(object):
            qualname = object.__qualname__
            # list of source code lines
            source = ''.join(lines)
            # Calling read_many / compile can execute code.
            # Cache pst on first compile?
            hst = read_many(source, file, skip_shebang=True)
            pst = hy_compile(hst, module, filename=file, source=source)
            class_finder = inspect._ClassFinder(qualname)
            try:
                return class_finder.visit(pst)
            except (inspect.ClassFoundException,) as e:
                return (lines, e.args[0])
    else:
        # Non-Hy object
        return inspect_findsource(object)

def getcomments(object):
    """
    Get comments relating to an object's source code.

    First checks for Hy source, otherwise defers to the original `inspect.getcomments`.
    Returns None when the source can't be found.
    """
    if inspect.getfile(object).endswith('.hy'):
        # Roughly follows the logic of inspect.getcomments, but for Hy comments
        try:
            [lines, lnum] = findsource(object)
            comments = []
            if inspect.ismodule(object):
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
            # Look for a comment block preceding the object
            elif lnum > 0:
                # Look for comments above the object and work up.
                indent = inspect.indentsize(lines[lnum])
                end = lnum - 1
                while end >= 0 and lines[end].lstrip().startswith(';') and True:
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
        return inspect_getcomments(object)

def getsourcelines(object):
    """Return a list of source lines and starting line number for a Hy or python object.

    First checks for Hy source, otherwise defers to the original `inspect.getsourcelines`.
    The argument may be a module, class, method, function, traceback, frame,
    or code object.  The source code is returned as a list of the lines
    corresponding to the object and the line number indicates where in the
    original source file the first line of code was found.  An OSError is
    raised if the source code cannot be retrieved."""
    from hy import repr
    from hy.reader import read_many

    object = inspect.unwrap(object)
    lines, lnum = findsource(object)

    if inspect.istraceback(object):
        object = object.tb_frame

    # For module or frame that corresponds to module, return all source lines.
    if (inspect.ismodule(object) or (inspect.isframe(object) and object.f_code.co_name == "<module>")):
        return lines, 0
    # Everything but Hy classes already works with inspect. 
    # The inspect.getblock function relies on inspect.BlockFinder which
    # assumes python tokenization. So deal with this as a special case.
    elif inspect.getfile(object).endswith('.hy'):
        # Read the first form that was found using inspect.findsource.
        form = next(read_many(''.join(lines[lnum:]), skip_shebang=True))
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
    lines, lnum = hy_getsourcelines(object)
    return ''.join(lines)

def _inject():
    """Inject the custom functions into the `inspect` module."""
    inspect.getsourcefile = getsourcefile
    inspect.findsource = findsource
    inspect.getcomments = getcomments
    inspect.getsourcelines = getsourcelines
    inspect.getsource = getsource

def _uninject():
    """Replace the custom functions with `inspect`'s original ones."""
    inspect.getsourcefile = __hy_compat_getsourcefile
    inspect.findsource = inspect_findsource
    inspect.getcomments = inspect_getcomments
    inspect.getsourcelines = inspect_getsourcelines
    inspect.getsource = inspect_getsource

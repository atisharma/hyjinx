"""
Static API surface inspection — no imports, no side effects.

Parses Hy and Python source files to extract public definitions
(functions, classes, macros, top-level bindings) without importing
the module. Uses hy.reader.read_many for .hy files and ast.parse
for .py files.

This avoids triggering top-level code, DB connections, GPU init, etc.
"""

import ast
import sys
from pathlib import Path

import hy
from hy.models import Expression, List, Symbol, Keyword, Integer, String
from hy.reader import read_many
from toolz import first


# ---------------------------------------------------------------------------
# Internal: Hy form extraction
# ---------------------------------------------------------------------------

def _extract_params(params_form):
    """Extract parameter names from a Hy parameter list form."""
    names = []
    for p in params_form:
        if isinstance(p, Symbol):
            s = str(p)
            if s == "#*":
                names.append("*args")
            elif s == "#**":
                names.append("**kwargs")
            else:
                names.append(hy.unmangle(s))
        elif isinstance(p, List) and p and isinstance(p[0], Symbol):
            names.append(hy.unmangle(str(p[0])))
    return names


def _parse_defn(form):
    """Parse a defn form, handling :async, decorators."""
    idx = 1
    # skip :async keyword
    if idx < len(form) and isinstance(form[idx], Keyword):
        idx += 1
    # skip decorator list
    if idx < len(form) and isinstance(form[idx], List):
        idx += 1
    if idx + 1 >= len(form):
        return None
    name_sym = form[idx]
    params_form = form[idx + 1]
    if not isinstance(name_sym, Symbol):
        return None
    return {
        "kind": "defn",
        "name": hy.unmangle(str(name_sym)),
        "params": _extract_params(params_form) if isinstance(params_form, List) else [],
        "line": form.start_line or 1,
    }


def _parse_defmacro(form):
    """Parse a defmacro form."""
    if len(form) < 3:
        return None
    name_sym = form[1]
    params_form = form[2]
    if not isinstance(name_sym, Symbol):
        return None
    return {
        "kind": "defmacro",
        "name": hy.unmangle(str(name_sym)),
        "params": _extract_params(params_form) if isinstance(params_form, List) else [],
        "line": form.start_line or 1,
    }


def _parse_setv(form):
    """Parse a setv form. May produce multiple bindings."""
    results = []
    for i in range(1, len(form) - 1, 2):
        name_form = form[i]
        if isinstance(name_form, Symbol):
            results.append({
                "kind": "setv",
                "name": hy.unmangle(str(name_form)),
                "params": [],
                "line": form.start_line or 1,
            })
    return results


def _parse_hy_form(form):
    """Extract a definition from a single top-level Hy form.

    Returns a dict, a list of dicts (from setv), or None.
    """
    if not isinstance(form, Expression) or not form:
        return None
    head = form[0]
    if not isinstance(head, Symbol):
        return None
    head_str = str(head)

    if head_str in ("defn", "defn/a"):
        return _parse_defn(form)
    if head_str == "defmacro":
        return _parse_defmacro(form)
    if head_str == "defclass":
        name_sym = form[1] if len(form) > 1 else None
        if isinstance(name_sym, Symbol):
            return {
                "kind": "defclass",
                "name": hy.unmangle(str(name_sym)),
                "params": [],
                "line": form.start_line or 1,
            }
        return None
    if head_str == "setv":
        return _parse_setv(form)
    return None


# ---------------------------------------------------------------------------
# Internal: Python AST extraction
# ---------------------------------------------------------------------------

def _parse_py_def(node):
    """Extract a Python function definition from an AST node."""
    params = [a.arg for a in node.args.args]
    return {
        "kind": "defn" if isinstance(node, ast.AsyncFunctionDef) else "def",
        "name": node.name,
        "params": params,
        "line": node.lineno,
    }


def _parse_py_tree(tree):
    """Walk top-level Python AST nodes and extract definitions."""
    results = []
    for node in ast.iter_child_nodes(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            results.append(_parse_py_def(node))
        elif isinstance(node, ast.ClassDef):
            results.append({"kind": "class", "name": node.name, "params": [], "line": node.lineno})
        elif isinstance(node, ast.Assign):
            for t in node.targets:
                if isinstance(t, ast.Name) and not t.name.startswith("_"):
                    results.append({"kind": "setv", "name": t.name, "params": [], "line": node.lineno})
    return results


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def api_surface(path):
    """Return a list of definition dicts for a source file.

    Each dict has keys: kind, name, params, line.
    No imports are performed — purely static analysis.
    """
    p = Path(path)
    source = p.read_text()

    if p.suffix.endswith(".hy"):
        forms = list(read_many(source))
        results = []
        for f in forms:
            defn = _parse_hy_form(f)
            if defn is not None:
                if isinstance(defn, list):
                    results.extend(defn)
                else:
                    results.append(defn)
        return results
    elif p.suffix.endswith(".py"):
        return _parse_py_tree(ast.parse(source, filename=str(p)))
    else:
        raise ValueError(f"Unsupported file type: {p.suffix}")


def resolve_module_path(dotted_name):
    """Resolve a dotted module name to a source file path, without importing.

    Walks sys.path manually to avoid triggering module side effects.
    Falls back to importlib.util.find_spec for editable installs (which
    may trigger parent package imports).
    Returns a Path or None.
    """
    parts = dotted_name.split(".")
    candidates = []

    # First try: walk sys.path without importing anything
    for base in sys.path:
        if not base or base.startswith("_"):
            continue
        package_dir = Path(base)
        if not package_dir.is_dir():
            continue
        for p in parts[:-1]:
            package_dir = package_dir / p
        if not package_dir.is_dir():
            continue
        base_name = parts[-1]
        # .hy first
        hy_file = package_dir / f"{base_name}.hy"
        if hy_file.exists():
            candidates.append(hy_file)
        # .py
        py_file = package_dir / f"{base_name}.py"
        if py_file.exists():
            candidates.append(py_file)
        # package __init__
        init_dir = package_dir / base_name
        if init_dir.is_dir():
            init_hy = init_dir / "__init__.hy"
            if init_hy.exists():
                candidates.append(init_hy)
            init_py = init_dir / "__init__.py"
            if init_py.exists():
                candidates.append(init_py)

    if candidates:
        return candidates[0]

    # Fallback: find_spec works for editable installs but may
    # trigger parent package __init__.py side effects.
    try:
        from importlib.util import find_spec
        spec = find_spec(dotted_name)
        if spec and spec.origin:
            origin = spec.origin
            hy_path = Path(origin.replace("__pycache__", "")).with_suffix(".hy")
            if hy_path.exists():
                return hy_path
            if Path(origin).exists():
                return Path(origin)
    except (ImportError, ModuleNotFoundError, ValueError):
        pass

    return None


def format_surface(defs, *, show_params=True, show_line=False):
    """Format a list of definition dicts as a readable table."""
    kind_tags = {
        "defn": "defn",
        "def": "def ",
        "defclass": "cls ",
        "class": "cls ",
        "defmacro": "mac ",
        "setv": "var ",
    }
    lines = []
    for d in defs:
        kind = d["kind"]
        name = d["name"]
        params = d["params"]
        line = d["line"]
        tag = kind_tags.get(kind, "    ")
        joined = ", ".join(params) if (show_params and params) else ""
        param_str = f"({joined})" if joined else ""
        line_str = f"  :{line}" if show_line else ""
        lines.append(f"  {tag} {name}{param_str}{line_str}")
    return "\n".join(lines)

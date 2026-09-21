"""Tests for the lazy package ``__init__`` (PEP 562).

The package used to import every heavy submodule eagerly, so ``import hyjinx``
paid for ``hyjinx.lib`` -> ``hyrule``, ``hyjinx.source`` -> ``beautifhy`` and
numpy, even for a caller that wanted only a toolz re-export or a macro.  These
tests observe the real import path in a clean subprocess; the laziness property
is invisible in-process once anything has imported a submodule.
"""

import importlib.util
import os
import subprocess
import sys
from pathlib import Path

import pytest

SUBMODULES = ("lib", "source", "docs", "result", "debug", "call_graph", "mat")

_REPO_ROOT = Path(__file__).resolve().parent.parent

_HAS_NUMPY = importlib.util.find_spec("numpy") is not None


def _run(code, env=None):
    """Run a snippet in a clean interpreter and return the CompletedProcess."""
    return subprocess.run(
        [sys.executable, "-c", code],
        capture_output=True,
        text=True,
        env=env,
        cwd=str(_REPO_ROOT),
    )


def _env(**overrides):
    env = dict(os.environ)
    env.update(overrides)
    return env


def test_import_hyjinx_does_not_load_submodules_or_hyrule():
    """``import hyjinx`` must not pull in a submodule or the macro runtime.

    The submodule check compares the DOTTED ``sys.modules`` keys, and the
    hyrule check is the regression tripwire: the eager ``__init__`` imported
    ``hyjinx.lib``, which requires hyrule.
    """
    code = (
        "import sys\n"
        "import hyjinx\n"
        f"submodules = {SUBMODULES!r}\n"
        "loaded = [m for m in sys.modules\n"
        "           if m in {'hyjinx.' + s for s in submodules}]\n"
        "assert not loaded, f'import hyjinx loaded {loaded}'\n"
        "assert 'hyrule' not in sys.modules, 'import hyjinx loaded hyrule'\n"
        "print('OK')\n"
    )
    result = _run(code, env=_env())
    assert result.returncode == 0, result.stderr
    assert "OK" in result.stdout


def test_eager_reexport_resolves_without_hyrule():
    """A toolz re-export is eager and must not drag in hyrule."""
    code = (
        "import sys\n"
        "import hyjinx\n"
        "assert hyjinx.first([1, 2, 3]) == 1\n"
        "assert list(hyjinx.take(2, [1, 2, 3])) == [1, 2]\n"
        "assert 'hyrule' not in sys.modules\n"
        "print('OK')\n"
    )
    result = _run(code, env=_env())
    assert result.returncode == 0, result.stderr
    assert "OK" in result.stdout


def test_lib_export_loads_lib_and_hyrule():
    """Touching a hyjinx.lib export imports the owning submodule and the
    macro runtime it requires, and only then."""
    code = (
        "import sys\n"
        "import hyjinx\n"
        "assert 'hyjinx.lib' not in sys.modules\n"
        "fn = hyjinx.jload\n"
        "assert callable(fn)\n"
        "assert 'hyjinx.lib' in sys.modules\n"
        "assert 'hyrule' in sys.modules\n"
        "print('OK')\n"
    )
    result = _run(code, env=_env())
    assert result.returncode == 0, result.stderr
    assert "OK" in result.stdout


def test_submodule_attribute_and_function_shadowing_preserved():
    """Bare submodule access resolves to the module where it did before, and
    ``call_graph`` still resolves to the function (not the submodule), matching
    the old ``from hyjinx.call_graph import call_graph`` shadowing."""
    code = (
        "import hyjinx\n"
        "import types\n"
        "assert isinstance(hyjinx.lib, types.ModuleType)\n"
        "assert isinstance(hyjinx.docs, types.ModuleType)\n"
        "assert callable(hyjinx.call_graph)\n"
        "assert not isinstance(hyjinx.call_graph, types.ModuleType)\n"
        "print('OK')\n"
    )
    result = _run(code, env=_env())
    assert result.returncode == 0, result.stderr
    assert "OK" in result.stdout


def test_every_lazy_export_resolves():
    """Every name in the lazy map resolves on the package."""
    import hyjinx

    for module_name, names in hyjinx._LAZY_EXPORTS.items():
        if module_name == "mat" and not _HAS_NUMPY:
            continue
        for name in names:
            # Raises AttributeError if the name is missing from the surface.
            getattr(hyjinx, name)


def test_unknown_attribute_raises():
    """A name outside the surface must not be silently resolved."""
    import hyjinx

    try:
        hyjinx.not_a_real_export
    except AttributeError:
        pass
    else:
        raise AssertionError("expected AttributeError for an unknown name")


def test_star_import_binds_every_name():
    """``from hyjinx import *`` still binds every ``__all__`` name."""
    import hyjinx

    namespace = {}
    exec("from hyjinx import *", namespace)
    missing = [n for n in hyjinx.__all__ if n not in namespace]
    assert not missing, f"star import missing {missing}"


def test_mat_degradation_matches_numpy():
    """mat is optional. With numpy absent its names must be absent from
    ``__all__`` and raise AttributeError, matching the old try/except import;
    with numpy present they must resolve normally."""
    import hyjinx

    if _HAS_NUMPY:
        assert "describe" in hyjinx.__all__
        assert callable(hyjinx.describe)
    else:
        assert "describe" not in hyjinx.__all__
        with pytest.raises(AttributeError):
            hyjinx.describe

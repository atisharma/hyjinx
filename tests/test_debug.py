"""Tests for the HyPdb debugger."""

import subprocess
import sys
import os
import tempfile
from textwrap import dedent

import pytest


HYJINX_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


class TestHyPdbCommands:
    """Tests for Hy-specific pdb commands."""

    def test_macroexpand_command(self, tmp_path):
        """Test macroexpand (me) command."""
        p = tmp_path / 'test.hy'
        p.write_text(dedent('''
            (defmacro my-add [a b] `(+ ~a ~b))
            (defn f []
              (breakpoint)
              (my-add 1 2))
            (f)''').strip())

        # Run hy with breakpoint and send commands
        result = subprocess.run(
            ['hy', str(p)],
            input='me (my-add 3 4)\nquit\n',
            capture_output=True,
            text=True,
        )

        # Should show the expanded form
        assert "'(+ 3 4)" in result.stdout or "(+ 3 4)" in result.stdout

    def test_macroexpand_1_command(self, tmp_path):
        """Test macroexpand_1 (me1) command."""
        p = tmp_path / 'test.hy'
        p.write_text(dedent('''
            (defn f []
              (breakpoint)
              42)
            (f)''').strip())

        result = subprocess.run(
            ['hy', str(p)],
            input='me1 (when True (print "hi"))\nquit\n',
            capture_output=True,
            text=True,
        )

        # when expands to if
        assert "(if True" in result.stdout or "'(if True" in result.stdout

    def test_macros_command(self, tmp_path):
        """Test macros (m) command."""
        p = tmp_path / 'test.hy'
        p.write_text(dedent('''
            (defmacro my-add [a b] `(+ ~a ~b))
            (defn f []
              (breakpoint)
              (my-add 1 2))
            (f)''').strip())

        result = subprocess.run(
            ['hy', str(p)],
            input='m\nquit\n',
            capture_output=True,
            text=True,
        )

        assert "my-add" in result.stdout

    def test_hy_repr_command(self, tmp_path):
        """Test hy_repr (hy) command."""
        p = tmp_path / 'test.hy'
        p.write_text(dedent('''
            (defn f []
              (setv result (+ 1 2))
              (breakpoint)
              result)
            (f)''').strip())

        result = subprocess.run(
            ['hy', str(p)],
            input='n\nhy result\nquit\n',
            capture_output=True,
            text=True,
        )

        # result should be 3
        assert "3" in result.stdout


class TestHyPdbImport:
    """Tests for module import."""

    def test_import_debug_module(self):
        """Test that debug module can be imported."""
        # Import just the module file without triggering __init__.py
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "hyjinx.debug",
            os.path.join(HYJINX_DIR, "hyjinx", "debug.py")
        )
        debug = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(debug)

        assert hasattr(debug, 'HyPdb')
        assert hasattr(debug, 'set_trace')

    def test_hypdb_has_commands(self):
        """Test that HyPdb has the expected commands."""
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "hyjinx.debug",
            os.path.join(HYJINX_DIR, "hyjinx", "debug.py")
        )
        debug = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(debug)

        # Check for Hy-specific commands
        assert hasattr(debug.HyPdb, 'do_macroexpand')
        assert hasattr(debug.HyPdb, 'do_macroexpand_1')
        assert hasattr(debug.HyPdb, 'do_macros')
        assert hasattr(debug.HyPdb, 'do_hy_repr')

        # Check for aliases
        assert hasattr(debug.HyPdb, 'do_me')
        assert hasattr(debug.HyPdb, 'do_me1')
        assert hasattr(debug.HyPdb, 'do_m')
        assert hasattr(debug.HyPdb, 'do_hy')

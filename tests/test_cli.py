"""
Tests for the hyjinx CLI.
"""

import subprocess
import sys


def run_cli(*args):
    """Run the hyjinx CLI with given arguments."""
    result = subprocess.run(
        [sys.executable, "-m", "hyjinx.cli"] + list(args),
        capture_output=True,
        text=True,
    )
    return result


class TestWhere:
    """Tests for the 'where' command."""

    def test_where_function(self):
        """Test finding a function's location."""
        result = run_cli("where", "json.dumps")
        assert result.returncode == 0
        assert "json" in result.stdout
        assert ":" in result.stdout  # file:line format

    def test_where_module(self):
        """Test finding a module's location."""
        result = run_cli("where", "hyjinx.macros")
        assert result.returncode == 0
        assert "macros.hy" in result.stdout

    def test_where_macro(self):
        """Test finding a macro's location."""
        result = run_cli("where", "hyjinx.macros.defmethod")
        assert result.returncode == 0
        assert "macros.hy" in result.stdout

    def test_where_multimethod(self):
        """Test finding a multimethod's location."""
        result = run_cli("where", "hyjinx.source.print-source")
        assert result.returncode == 0
        assert "source.hy" in result.stdout

    def test_where_not_found(self):
        """Test error for non-existent symbol."""
        result = run_cli("where", "nonexistent.module")
        assert result.returncode == 1
        assert "not found" in result.stderr.lower() or "not found" in result.stdout.lower()

    def test_where_json_output(self):
        """Test JSON output format."""
        result = run_cli("where", "json.dumps", "--json")
        assert result.returncode == 0
        import json
        data = json.loads(result.stdout)
        assert "file" in data
        assert "line" in data
        assert "module" in data


class TestSource:
    """Tests for the 'source' command."""

    def test_source_function(self):
        """Test showing a function's source."""
        result = run_cli("source", "hyjinx.source.get-source-details")
        assert result.returncode == 0
        assert "defn" in result.stdout or "get_source_details" in result.stdout

    def test_source_module(self):
        """Test showing a module's source."""
        result = run_cli("source", "hyjinx.macros")
        assert result.returncode == 0
        assert "defmacro" in result.stdout or "macros" in result.stdout

    def test_source_macro(self):
        """Test showing a macro's source."""
        result = run_cli("source", "hyjinx.macros.defmethod")
        assert result.returncode == 0
        assert "defmacro" in result.stdout

    def test_source_multimethod(self):
        """Test showing a multimethod's source."""
        result = run_cli("source", "hyjinx.source.print-source")
        assert result.returncode == 0
        assert "defmethod" in result.stdout or "print_source" in result.stdout

    def test_source_not_found(self):
        """Test error for non-existent symbol."""
        result = run_cli("source", "nonexistent.module")
        assert result.returncode == 1
        assert "not found" in result.stderr.lower() or "not found" in result.stdout.lower()


class TestDoc:
    """Tests for the 'doc' command."""

    def test_doc_function(self):
        """Test showing a function's docstring."""
        result = run_cli("doc", "hyjinx.source.get-source-details")
        assert result.returncode == 0
        assert "dict" in result.stdout.lower() or "line" in result.stdout.lower()

    def test_doc_module(self):
        """Test showing a module's docstring."""
        result = run_cli("doc", "hyjinx.macros")
        assert result.returncode == 0
        assert "macros" in result.stdout.lower()

    def test_doc_macro(self):
        """Test showing a macro's docstring."""
        result = run_cli("doc", "hyjinx.macros.defmethod")
        assert result.returncode == 0
        assert "multimethod" in result.stdout.lower()

    def test_doc_not_found(self):
        """Test error for non-existent symbol."""
        result = run_cli("doc", "nonexistent.module")
        assert result.returncode == 1
        assert "not found" in result.stderr.lower() or "not found" in result.stdout.lower()

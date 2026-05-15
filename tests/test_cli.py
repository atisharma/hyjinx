"""
Tests for the hyjinx CLI.
"""

import subprocess
import sys
import os


HYJINX_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


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


class TestDir:
    """Tests for the 'dir' command."""

    def test_dir_absolute_path(self):
        """Test dir with absolute path to a .hy file."""
        result = run_cli("dir", os.path.join(HYJINX_DIR, "hyjinx", "api.hy"))
        assert result.returncode == 0
        assert "api-surface" in result.stdout
        assert "definitions" in result.stdout

    def test_dir_dotted_module(self):
        """Test dir with dotted module name."""
        result = run_cli("dir", "hyjinx.api")
        assert result.returncode == 0
        assert "api-surface" in result.stdout

    def test_dir_python_file(self):
        """Test dir with a Python file."""
        result = run_cli("dir", os.path.join(HYJINX_DIR, "hyjinx", "hjx_inspect.py"))
        assert result.returncode == 0
        assert "getfile" in result.stdout

    def test_dir_with_line_numbers(self):
        """Test dir with --line flag."""
        result = run_cli("dir", "hyjinx.api", "--line")
        assert result.returncode == 0
        assert ":" in result.stdout  # line numbers have : prefix

    def test_dir_no_params(self):
        """Test dir with --no-params flag hides parameter names."""
        result = run_cli("dir", "hyjinx.api", "--no-params")
        assert result.returncode == 0
        assert "api-surface" in result.stdout
        assert "(" not in result.stdout  # no params shown

    def test_dir_kind_filter(self):
        """Test dir with --kind filter."""
        result = run_cli("dir", "hyjinx.api", "--kind", "defn")
        assert result.returncode == 0
        assert "defn" in result.stdout
        # 'var ' tag is used for setv, not 'setv' — check tag not in output
        assert "var " not in result.stdout

    def test_dir_nonexistent_module(self):
        """Test dir with a module that doesn't exist."""
        result = run_cli("dir", "nonexistent.module.xyz")
        assert result.returncode == 1

    def test_dir_nonexistent_file(self):
        """Test dir with a file that doesn't exist."""
        result = run_cli("dir", "/nonexistent/path/file.hy")
        assert result.returncode == 1

    def test_dir_shebang_file(self):
        """Test dir on a Hy file with a shebang line (cli.hy)."""
        result = run_cli("dir", "hyjinx.cli")
        assert result.returncode == 0
        assert "resolve-symbol" in result.stdout

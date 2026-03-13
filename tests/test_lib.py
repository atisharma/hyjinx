"""
Tests for hyjinx.lib — the utility function library.

Covers: string ops, numeric ops, data structure helpers,
file I/O (slurp/spit, jload/jsave), and the Result-based
variants added in the Result integration (slurp-result,
jload-result, extract-json).
"""

import json
import os
import pytest
import tempfile
import hy  # must precede any hyjinx import

import hyjinx.lib as lib
from hyjinx.result import (
    hyx_okXquestion_markX as ok_p,
    hyx_errXquestion_markX as err_p,
    unwrap,
)


# ── String helpers ────────────────────────────────────────────────────────────

class TestStringHelpers:
    def test_sstrip_strips_whitespace_and_quotes(self):
        assert lib.sstrip("  Hello.  ") == "hello"

    def test_sstrip_removes_the_prefix(self):
        assert lib.sstrip("The quick brown fox") == "quick brown fox"

    def test_grepp_returns_matching_lines(self):
        lines = ["apple", "banana", "apricot", "cherry"]
        result = lib.grepp("^a", lines)
        assert result == ["apple", "apricot"]

    def test_grepp_on_string(self):
        text = "line1\nline2\napple"
        result = lib.grepp("apple", text)
        assert result == ["apple"]

    def test_grepp_line_nos(self):
        lines = ["one", "two", "three"]
        result = lib.grepp("two", lines, line_nos=True)
        assert len(result) == 1
        assert "0001" in result[0]

    def test_is_url_valid(self):
        assert lib.is_url("https://example.com") is True
        assert lib.is_url("http://foo.bar/baz") is True

    def test_is_url_invalid(self):
        assert lib.is_url("not-a-url") is False
        assert lib.is_url("") is False

    def test_get_numeric_extracts_numbers(self):
        nums = lib.get_numeric("price: 3.14 and -42 items")
        assert 3.14 in nums
        assert -42.0 in nums


# ── Numeric helpers ───────────────────────────────────────────────────────────

class TestNumericHelpers:
    def test_as_float_valid(self):
        assert lib.as_float("3.14") == pytest.approx(3.14)
        assert lib.as_float(42) == 42.0

    def test_as_float_invalid_returns_nan(self):
        import math
        result = lib.as_float("banana")
        assert math.isnan(result)

    def test_isnumeric_true(self):
        assert lib.isnumeric("3.14") is True
        assert lib.isnumeric(0) is True

    def test_isnumeric_false(self):
        assert lib.isnumeric("abc") is False
        assert lib.isnumeric(None) is False

    def test_sign_positive(self):
        assert lib.sign(5) == 1.0
        assert lib.sign(0) == 1.0

    def test_sign_negative(self):
        assert lib.sign(-3) == -1.0

    def test_round_to(self):
        assert lib.round_to(17, 5) == 15
        assert lib.round_to(23, 10) == 20

    def test_pos_neg_zero(self):
        assert lib.hyx_posXquestion_markX(1) is True
        assert lib.hyx_posXquestion_markX(0) is True
        assert lib.hyx_negXquestion_markX(-1) is True
        assert lib.hyx_zeroXquestion_markX(0) is True
        assert lib.hyx_zeroXquestion_markX(1) is False


# ── Data structure helpers ────────────────────────────────────────────────────

class TestDataStructures:
    def test_get_in_nested(self):
        d = {"a": {"b": {"c": 42}}}
        assert lib.get_in(d, "a", "b", "c") == 42

    def test_get_in_missing_returns_none(self):
        d = {"a": {"b": 1}}
        assert lib.get_in(d, "a", "x", "y") is None

    def test_get_in_empty_path(self):
        d = {"a": 1}
        assert lib.get_in(d) == d

    def test_sieve_removes_none(self):
        result = list(lib.sieve([1, None, 2, None, 3]))
        assert result == [1, 2, 3]

    def test_group(self):
        groups = list(lib.group([1, 2, 3, 4, 5], 2))
        assert groups == [(1, 2), (3, 4), (5,)]

    def test_short_id_deterministic(self):
        a = lib.short_id("hello")
        b = lib.short_id("hello")
        assert a == b
        assert len(a) == 6

    def test_hash_id_is_hex(self):
        h = lib.hash_id("test")
        assert all(c in "0123456789abcdef" for c in h)


# ── File I/O: slurp / spit ────────────────────────────────────────────────────

class TestSlurpSpit:
    def test_roundtrip(self, tmp_path):
        f = tmp_path / "test.txt"
        lib.spit(str(f), "hello world")
        assert lib.slurp(str(f)) == "hello world"

    def test_spit_append_mode(self, tmp_path):
        f = tmp_path / "log.txt"
        lib.spit(str(f), "line1\n")
        lib.spit(str(f), "line2\n", mode="a")
        content = lib.slurp(str(f))
        assert "line1" in content
        assert "line2" in content

    def test_slurp_missing_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            lib.slurp(str(tmp_path / "nonexistent.txt"))


# ── slurp-result (Result integration) ────────────────────────────────────────

class TestSlurpResult:
    def test_ok_on_existing_file(self, tmp_path):
        f = tmp_path / "data.txt"
        f.write_text("content here")
        r = lib.slurp_result(str(f))
        assert ok_p(r)
        assert unwrap(r) == "content here"

    def test_err_on_missing_file(self, tmp_path):
        r = lib.slurp_result(str(tmp_path / "ghost.txt"))
        assert err_p(r)
        assert r["error"]["type"] == "file-not-found"
        assert str(tmp_path / "ghost.txt") in r["error"]["context"]["path"]

    def test_err_contains_path(self, tmp_path):
        path = str(tmp_path / "missing.txt")
        r = lib.slurp_result(path)
        assert r["error"]["context"]["path"] == path


# ── JSON I/O: jload / jsave ───────────────────────────────────────────────────

class TestJloadJsave:
    def test_roundtrip(self, tmp_path):
        f = str(tmp_path / "data.json")
        obj = {"key": "value", "num": 42}
        lib.jsave(obj, f)
        loaded = lib.jload(f)
        assert loaded == obj

    def test_jload_missing_returns_none(self, tmp_path):
        result = lib.jload(str(tmp_path / "nope.json"))
        assert result is None


# ── jload-result (Result integration) ────────────────────────────────────────

class TestJloadResult:
    def test_ok_on_valid_json(self, tmp_path):
        f = tmp_path / "data.json"
        f.write_text('{"x": 1}')
        r = lib.jload_result(str(f))
        assert ok_p(r)
        assert unwrap(r) == {"x": 1}

    def test_err_file_not_found(self, tmp_path):
        r = lib.jload_result(str(tmp_path / "missing.json"))
        assert err_p(r)
        assert r["error"]["type"] == "file-not-found"

    def test_err_invalid_json(self, tmp_path):
        f = tmp_path / "bad.json"
        f.write_text("this is not json {{{")
        r = lib.jload_result(str(f))
        assert err_p(r)
        assert r["error"]["type"] == "json-decode-error"
        assert r["error"]["context"]["path"] == str(f)


# ── extract-json (refactored to return Result) ────────────────────────────────

class TestExtractJson:
    def test_ok_on_valid_object(self):
        r = lib.extract_json('prefix {"key": "value"} suffix')
        assert ok_p(r)
        assert unwrap(r) == {"key": "value"}

    def test_ok_on_valid_array(self):
        r = lib.extract_json('result: [1, 2, 3]')
        assert ok_p(r)
        assert unwrap(r) == [1, 2, 3]

    def test_err_no_json(self):
        r = lib.extract_json("plain text with no json content")
        assert err_p(r)
        assert r["error"]["type"] == "no-json-found"

    def test_err_malformed_json(self):
        r = lib.extract_json('result: {broken json here')
        # The regex finds "{broken json here" but JSON.loads fails
        # Depending on the regex, it may match partial content
        # If it finds a match but can't parse: json-decode-error
        # If it finds no valid braces match: no-json-found
        # Either error is acceptable; what's NOT acceptable is ok with garbage
        assert err_p(r)

    def test_empty_string(self):
        r = lib.extract_json("")
        assert err_p(r)
        assert r["error"]["type"] == "no-json-found"

    def test_nested_object(self):
        text = 'data: {"a": {"b": [1, 2]}, "c": true}'
        r = lib.extract_json(text)
        assert ok_p(r)
        data = unwrap(r)
        assert data["a"]["b"] == [1, 2]

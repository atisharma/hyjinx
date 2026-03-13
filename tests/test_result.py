"""
Tests for hyjinx.result — the Result type for explicit error handling.

Covers: constructors, predicates, threading macros, binding macros,
exception bridging, and utilities.
"""

import pytest
import hy  # must be imported before any hyjinx.result import

from hyjinx.result import (
    ok, err,
    hyx_okXquestion_markX as ok_p,
    hyx_errXquestion_markX as err_p,
    hyx_resultXquestion_markX as result_p,
    as_result,
    unwrap,
    unwrap_or,
    map_ok,
    map_err,
    collect_results,
)

# Require the Hy macros so we can exercise them via hy.eval in tests
# The threading/binding macros are tested via hy.eval snippets below.


# ── Constructors ─────────────────────────────────────────────────────────────

class TestConstructors:
    def test_ok_shape(self):
        r = ok(42)
        assert r == {"ok": True, "value": 42}

    def test_ok_none(self):
        r = ok(None)
        assert r["ok"] is True
        assert r["value"] is None

    def test_err_shape(self):
        r = err("validation-error", "must be positive", {"field": "x"})
        assert r["ok"] is False
        assert r["error"]["type"] == "validation-error"
        assert r["error"]["message"] == "must be positive"
        assert r["error"]["context"] == {"field": "x"}

    def test_err_default_context(self):
        r = err("some-error", "message")
        assert r["error"]["context"] == {}

    def test_ok_wraps_any_value(self):
        for v in [0, "", [], {}, False, 3.14]:
            assert ok(v)["value"] == v


# ── Predicates ───────────────────────────────────────────────────────────────

class TestPredicates:
    def test_ok_p_true_for_ok(self):
        assert ok_p(ok(1)) is True

    def test_ok_p_false_for_err(self):
        assert ok_p(err("e", "msg")) is False

    def test_ok_p_false_for_plain_dict(self):
        assert ok_p({"foo": "bar"}) is False

    def test_err_p_true_for_err(self):
        assert err_p(err("e", "msg")) is True

    def test_err_p_false_for_ok(self):
        assert err_p(ok(99)) is False

    def test_result_p_true_for_ok(self):
        assert result_p(ok(1)) is True

    def test_result_p_true_for_err(self):
        assert result_p(err("e", "m")) is True

    def test_result_p_false_for_plain(self):
        assert result_p({"x": 1}) is False
        assert result_p(42) is False
        assert result_p("string") is False

    def test_as_result_passthrough(self):
        r = ok(5)
        assert as_result(r) is r

    def test_as_result_wraps_plain(self):
        r = as_result(99)
        assert ok_p(r)
        assert r["value"] == 99


# ── Utilities ─────────────────────────────────────────────────────────────────

class TestUtilities:
    def test_unwrap_ok(self):
        assert unwrap(ok("hello")) == "hello"

    def test_unwrap_err_raises(self):
        with pytest.raises(AssertionError):
            unwrap(err("e", "msg"))

    def test_unwrap_or_ok(self):
        assert unwrap_or(ok(10), 99) == 10

    def test_unwrap_or_err(self):
        assert unwrap_or(err("e", "msg"), 99) == 99

    def test_map_ok_transforms_value(self):
        r = map_ok(lambda x: x * 2, ok(5))
        assert ok_p(r)
        assert r["value"] == 10

    def test_map_ok_passthrough_err(self):
        e = err("e", "msg")
        assert map_ok(lambda x: x * 2, e) is e

    def test_map_ok_function_returning_result(self):
        r = map_ok(lambda x: ok(x + 1), ok(5))
        assert ok_p(r)
        assert r["value"] == 6

    def test_map_err_transforms_error(self):
        e = err("old-type", "old message")
        r = map_err(lambda e: {**e, "type": "new-type"}, e)
        assert err_p(r)
        assert r["error"]["type"] == "new-type"

    def test_map_err_passthrough_ok(self):
        o = ok(42)
        assert map_err(lambda e: e, o) is o

    def test_collect_results_all_ok(self):
        results = [ok(1), ok(2), ok(3)]
        r = collect_results(results)
        assert ok_p(r)
        assert r["value"] == [1, 2, 3]

    def test_collect_results_first_err(self):
        results = [ok(1), err("e", "boom"), ok(3)]
        r = collect_results(results)
        assert err_p(r)
        assert r["error"]["message"] == "boom"

    def test_collect_results_empty(self):
        r = collect_results([])
        assert ok_p(r)
        assert r["value"] == []


# ── Hy macro integration via hy.eval ─────────────────────────────────────────

class TestHyMacros:
    """
    Test the threading and binding macros by evaluating small Hy snippets.
    These macros must be exercised in Hy because they use Hy syntax.
    """

    def _eval(self, code: str):
        """Evaluate a Hy string in a fresh context with result macros available.

        result-> and result->> internally use hyrule's -> / ->>, so we require
        hyrule threading macros alongside the result macros.
        """
        full = f"""
(require hyrule [-> ->>])
(require hyjinx.result [result-> result->> let-result match-result try-result])
(import hyjinx.result [ok err
                        hyx-okXquestion-markX :as ok?
                        hyx-errXquestion-markX :as err?
                        unwrap unwrap-or])
{code}
"""
        return hy.eval(hy.read_many(full))

    def test_result_arrow_short_circuits_on_err(self):
        result = self._eval("""
(defn bad-step [x] (err "fail" "exploded"))
(defn double [x] (* x 100))
(result-> (ok 1) bad-step double)
""")
        assert err_p(result)
        assert result["error"]["message"] == "exploded"

    def test_result_arrow_propagates_value(self):
        # Threading ops: value is inserted as first arg to each form.
        # (result-> (ok 3) (+ 1) (* 2)) expands:
        #   3 -> (+ 3 1) = 4 -> (* 4 2) = 8
        result = self._eval("""
(result-> (ok 3) (+ 1) (* 2))
""")
        assert ok_p(result)
        assert result["value"] == 8

    def test_result_arrow_wraps_plain_returns(self):
        # Named function call — value passed as first arg.
        result = self._eval("""
(defn triple [x] (* x 3))
(result-> 5 triple)
""")
        assert ok_p(result)
        assert result["value"] == 15

    def test_let_result_binds_value(self):
        result = self._eval("""
(let-result [x (ok 42)]
  (ok (* x 2)))
""")
        assert ok_p(result)
        assert result["value"] == 84

    def test_let_result_propagates_err(self):
        result = self._eval("""
(let-result [x (err "fail" "no value")]
  (ok (* x 2)))
""")
        assert err_p(result)
        assert result["error"]["message"] == "no value"

    def test_match_result_ok_branch(self):
        result = self._eval("""
(match-result (ok 10)
  v  (* v 3)
  e  -1)
""")
        assert result == 30

    def test_match_result_err_branch(self):
        result = self._eval("""
(match-result (err "e" "bad input")
  v  (* v 3)
  e  (get e "message"))
""")
        assert result == "bad input"

    def test_try_result_success(self):
        result = self._eval("""
(try-result (int "42"))
""")
        assert ok_p(result)
        assert result["value"] == 42

    def test_try_result_exception(self):
        result = self._eval("""
(try-result (int "not-a-number"))
""")
        assert err_p(result)
        assert result["error"]["type"] == "ValueError"

    def test_result_darr_threading(self):
        result = self._eval("""
(defn parse-int [s] (try-result (int s)))
(defn double-it [n] (ok (* n 2)))
(result-> "21" parse-int double-it)
""")
        assert ok_p(result)
        assert result["value"] == 42

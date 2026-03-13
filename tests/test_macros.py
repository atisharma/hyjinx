"""
Tests for hyjinx.macros — the macro collection.

Covers: defmethod (multimethod dispatch), defstruct (frozen dataclass),
when-let, do-while, and sequence macros (lmap, .., prepend, append).
"""

import types
import pytest
import hy
import hy.macros
import hyjinx.macros  # noqa: F401 — triggers compilation of the Hy module


def make_module():
    """Create a fresh module with hyjinx.macros required."""
    m = types.ModuleType("__hy_test__")
    hy.macros.require("hyjinx.macros", m, assignments="ALL", prefix="")
    return m


def hy_eval(code: str):
    """Evaluate Hy code with all hyjinx.macros available."""
    m = make_module()
    return hy.eval(hy.read_many(code), module=m)


# ── defmethod (multimethod dispatch) ─────────────────────────────────────────

class TestDefmethod:
    def test_dispatch_on_int(self):
        result = hy_eval("""
(defmethod classify [#^ int x]
  "integer")
(defmethod classify [#^ str x]
  "string")
(classify 42)
""")
        assert result == "integer"

    def test_dispatch_on_str(self):
        result = hy_eval("""
(defmethod classify2 [#^ int x]
  "integer")
(defmethod classify2 [#^ str x]
  "string")
(classify2 "hello")
""")
        assert result == "string"

    def test_dispatch_accumulates_multimethods(self):
        result_int = hy_eval("""
(defmethod adder [#^ int x] (+ x 1))
(defmethod adder [#^ float x] (+ x 0.5))
(adder 10)
""")
        result_float = hy_eval("""
(defmethod adder3 [#^ int x] (+ x 1))
(defmethod adder3 [#^ float x] (+ x 0.5))
(adder3 1.0)
""")
        assert result_int == 11
        assert result_float == pytest.approx(1.5)


# ── defstruct (frozen dataclass) ─────────────────────────────────────────────

class TestDefstruct:
    def test_basic_struct(self):
        Point = hy_eval("""
(defstruct Point
  #^ int x
  #^ int y)
Point
""")
        p = Point(x=1, y=2)
        assert p.x == 1
        assert p.y == 2

    def test_struct_is_frozen(self):
        Point = hy_eval("""
(defstruct ImmutablePoint
  #^ int x
  #^ int y)
ImmutablePoint
""")
        p = Point(x=3, y=4)
        with pytest.raises((AttributeError, TypeError)):
            p.x = 99

    def test_struct_equality(self):
        Point = hy_eval("""
(defstruct EqPoint
  #^ int x
  #^ int y)
EqPoint
""")
        assert Point(x=1, y=2) == Point(x=1, y=2)
        assert Point(x=1, y=2) != Point(x=1, y=3)


# ── when-let ──────────────────────────────────────────────────────────────────

class TestWhenLet:
    def test_executes_body_when_truthy(self):
        result = hy_eval("""
(when-let [x 42]
  (* x 2))
""")
        assert result == 84

    def test_returns_none_when_falsy(self):
        result = hy_eval("""
(when-let [x None]
  "should not reach here")
""")
        assert result is None

    def test_returns_none_on_zero(self):
        result = hy_eval("""
(when-let [x 0]
  "executed")
""")
        assert result is None  # 0 is falsy

    def test_with_dict_lookup(self):
        result = hy_eval("""
(setv d {"key" "value"})
(when-let [v (.get d "key")]
  (.upper v))
""")
        assert result == "VALUE"


# ── Sequence macros ───────────────────────────────────────────────────────────

class TestSequenceMacros:
    def test_lmap_is_eager(self):
        result = hy_eval("""
(lmap (fn [x] (* x 2)) [1 2 3])
""")
        assert result == [2, 4, 6]
        assert isinstance(result, list)

    def test_range_macro(self):
        result = hy_eval("""
(.. 0 5)
""")
        assert result == [0, 1, 2, 3, 4]

    def test_range_macro_with_step(self):
        result = hy_eval("""
(.. 0 10 2)
""")
        assert result == [0, 2, 4, 6, 8]

    def test_prepend(self):
        result = hy_eval("""
(prepend 0 [1 2 3])
""")
        assert result == [0, 1, 2, 3]

    def test_append(self):
        result = hy_eval("""
(append 4 [1 2 3])
""")
        assert result == [1, 2, 3, 4]


# ── do-while ──────────────────────────────────────────────────────────────────

class TestDoWhile:
    def test_executes_body_at_least_once(self):
        result = hy_eval("""
(setv counter 0)
(do-while False
  (setv counter (+ counter 1)))
counter
""")
        assert result == 1  # ran once despite condition being False

    def test_loops_while_true(self):
        result = hy_eval("""
(setv n 0)
(do-while (< n 3)
  (setv n (+ n 1)))
n
""")
        assert result == 3

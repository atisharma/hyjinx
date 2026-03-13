"""
Tests for hyjinx.actors — the asyncio actor framework.

Covers: Actor lifecycle (start/stop), send (fire-and-forget),
ask (request-reply), defactor macro, error propagation,
status reporting, and async context manager usage.
"""

import asyncio
import pytest
import hy  # must precede hyjinx imports

from hyjinx.actors import Actor


# ── Concrete actor implementations for testing ────────────────────────────────

class EchoActor(Actor):
    """Returns whatever it receives."""
    async def recv(self, message):
        return message


class UpperActor(Actor):
    """Uppercases string messages."""
    async def recv(self, message):
        return message.upper()


class AddingActor(Actor):
    """Expects a tuple (a, b) and returns their sum."""
    async def recv(self, message):
        a, b = message
        return a + b


class BoomActor(Actor):
    """Raises ValueError for every message."""
    async def recv(self, message):
        raise ValueError(f"boom: {message}")


class SlowActor(Actor):
    """Simulates I/O with a brief sleep."""
    async def recv(self, message):
        await asyncio.sleep(0.01)
        return f"processed: {message}"


# ── Helpers ───────────────────────────────────────────────────────────────────

async def run_with_actor(actor_cls, coro_fn):
    """Start an actor, run coro_fn(actor), stop it."""
    actor = actor_cls()
    actor.start()
    try:
        return await coro_fn(actor)
    finally:
        await actor.stop()


# ── Lifecycle ─────────────────────────────────────────────────────────────────

class TestActorLifecycle:
    def test_start_sets_started_flag(self):
        async def _test():
            a = EchoActor()
            a.start()
            assert a._started is True
            await a.stop()
        asyncio.run(_test())

    def test_double_start_raises(self):
        async def _test():
            a = EchoActor()
            a.start()
            with pytest.raises(RuntimeError, match="already started"):
                a.start()
            await a.stop()
        asyncio.run(_test())

    def test_stop_clears_started_flag(self):
        async def _test():
            a = EchoActor()
            a.start()
            await a.stop()
            assert a._started is False
        asyncio.run(_test())

    def test_send_before_start_raises(self):
        async def _test():
            a = EchoActor()
            with pytest.raises(RuntimeError, match="not started"):
                await a.send("hello")
        asyncio.run(_test())

    def test_ask_before_start_raises(self):
        async def _test():
            a = EchoActor()
            with pytest.raises(RuntimeError, match="not started"):
                await a.ask("hello")
        asyncio.run(_test())

    def test_context_manager(self):
        async def _test():
            async with EchoActor() as actor:
                result = await actor.ask("hello")
            return result
        result = asyncio.run(_test())
        assert result == "hello"

    def test_status_stopped(self):
        a = EchoActor()
        assert a.status["state"] == "stopped"

    def test_status_running(self):
        async def _test():
            a = EchoActor()
            a.start()
            status = a.status
            await a.stop()
            return status
        status = asyncio.run(_test())
        assert status["state"] == "running"


# ── send (fire-and-forget) ────────────────────────────────────────────────────

class TestSend:
    def test_send_does_not_raise(self):
        async def _test():
            async with EchoActor() as actor:
                await actor.send("test message")
        asyncio.run(_test())

    def test_send_with_exception_does_not_propagate(self):
        """BoomActor always raises; send should absorb it silently."""
        async def _test():
            async with BoomActor() as actor:
                await actor.send("trigger")
                await asyncio.sleep(0.05)  # let the handler run
        asyncio.run(_test())  # must not raise


# ── ask (request-reply) ───────────────────────────────────────────────────────

class TestAsk:
    def test_ask_returns_value(self):
        async def _test():
            async with EchoActor() as actor:
                return await actor.ask("ping")
        assert asyncio.run(_test()) == "ping"

    def test_ask_upper(self):
        async def _test():
            async with UpperActor() as actor:
                return await actor.ask("hello world")
        assert asyncio.run(_test()) == "HELLO WORLD"

    def test_ask_with_tuple(self):
        async def _test():
            async with AddingActor() as actor:
                return await actor.ask((3, 7))
        assert asyncio.run(_test()) == 10

    def test_ask_propagates_exception(self):
        async def _test():
            async with BoomActor() as actor:
                return await actor.ask("oops")
        with pytest.raises(ValueError, match="boom: oops"):
            asyncio.run(_test())

    def test_ask_multiple_sequential(self):
        async def _test():
            async with AddingActor() as actor:
                results = []
                for i in range(5):
                    results.append(await actor.ask((i, i)))
                return results
        results = asyncio.run(_test())
        assert results == [0, 2, 4, 6, 8]

    def test_ask_slow_actor(self):
        async def _test():
            async with SlowActor() as actor:
                return await actor.ask("data")
        result = asyncio.run(_test())
        assert result == "processed: data"


# ── defactor macro ────────────────────────────────────────────────────────────

class TestDefactor:
    def test_defactor_creates_actor(self):
        """defactor macro generates an Actor subclass from a Hy snippet."""
        import hy as _hy
        Greeter = _hy.eval(_hy.read_many("""
(import hyjinx.actors [Actor])
(defmacro defactor [a args #* body]
  `(do
     (import hyjinx.actors [Actor])
     (defclass ~a [Actor]
       (defn :async recv [self ~@args]
         ~@body))))
(defactor Greeter [#^ str name]
  (.upper name))
Greeter
"""))
        async def _test():
            async with Greeter() as actor:
                return await actor.ask("alice")
        assert asyncio.run(_test()) == "ALICE"

    def test_defactor_via_import(self):
        """Use the macro as exposed by hyjinx.actors directly."""
        import hy as _hy
        Doubler = _hy.eval(_hy.read_many("""
(import hyjinx.actors [Actor])
(defmacro defactor [a args #* body]
  `(do
     (import hyjinx.actors [Actor])
     (defclass ~a [Actor]
       (defn :async recv [self ~@args]
         ~@body))))
(defactor Doubler [#^ int n]
  (* n 2))
Doubler
"""))
        async def _test():
            async with Doubler() as actor:
                return await actor.ask(21)
        assert asyncio.run(_test()) == 42


# ── Queue/status metrics ──────────────────────────────────────────────────────

class TestActorMetrics:
    def test_status_has_required_keys(self):
        async def _test():
            async with EchoActor() as actor:
                return actor.status
        status = asyncio.run(_test())
        assert "state" in status
        assert "metrics" in status
        assert "health" in status
        assert "running_tasks" in status["metrics"]
        assert "queue_size" in status["metrics"]

# AGENTS.md: cboe-sim/test

Expect tests (`let%expect_test`) and property tests for the venue, the wire codecs and the
anchoring. Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/runtest`.

Rules:
- Promote output only after reading the diff: `dune promote` after `@all` with `@runtest`
  built. Promoting after `@all` alone promotes nothing.
- Under `open! Async`, an expect test must return `unit Deferred.t`; keep helpers that
  need `Async` in a submodule or use `Thread_safe.block_on_async_exn`.
- Tests that depend on the clock use `Clock`'s manual mode, not wall time.
- Do not print file paths or host-specific text in expect output.

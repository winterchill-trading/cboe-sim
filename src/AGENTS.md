# AGENTS.md: cboe-sim/src

The `cboe-sim` library (module `Cboe_sim`): the venue on top of `exchange-core-engine`.

| Module | Holds |
| --- | --- |
| `Venue` | BOE semantics over the engine: FOK, hidden and reserve orders, auction uncrossing, PITCH events |
| `Boe_session` | Per-member outbound store (sequences per matching unit, kept for replay) and the connection state machine |
| `Feed` | PITCH sequencing per unit, Time messages, displayed book for spins, TCP and JSON fan-out |
| `Server`, `Net` | TCP gateway and feed, HTTP status and `/ws`, frame readers |
| `Clock` | Session clock: New York time, accelerated replay, always open, or manual for tests |
| `Instruments` | Equities on unit 1 and SPY option series on unit 2 |
| `Anchoring` | Wrapper over `Price_anchor.Anchor`, one per anchored listing |

Build and test: `opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/all` (add
`DUNE_BUILD_DIR=_build-cboe-sim` when working in parallel).

Rules:
- Name a module `Boe_session`, not `Session`: `Equities_core.Session` is opened in the same
  library.
- `Anchoring` must keep `step_clock:false` and `levels:0` when it pulls on the exchange
  clock; changing either moves the engine's time away from the venue's.
- Every anchored listing is clamped to its LULD bands in `Venue`; do not bypass it.
- Message encoders stay in `wire/`; this directory only decides what to send.
- Feed frames are numbered per unit; a gap or reset must go through `Feed`, not be
  handled in `Server`.

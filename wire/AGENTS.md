# AGENTS.md: cboe-sim/wire

The `cboe-sim.wire` library (module `Cboe_wire`): byte-level codecs, no I/O.

| Module | Holds |
| --- | --- |
| `Boe` | BOE v2 framing, session and in-scope application messages; prices with 4 implied decimals (`Price4`) |
| `Pitch` | Multicast PITCH 2.X messages and the Sequenced Unit Header over a TCP stream |
| `Pitch_book` | Order-by-order book rebuilt from PITCH (spin, then live), also used to serve spins |
| `Bitfields`, `Le`, `Pretty` | Bit access, little-endian reads and writes, debug printers |

Build and test: `opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/all`.

Rules:
- Layouts follow the summaries in `../../equities-core/docs/cboe-boe.md` and
  `../../equities-core/docs/cboe-pitch.md`. Cite the section in a comment when a field
  is non-obvious.
- Round-trip every new message in `test/` (encode, decode, compare) before wiring it into
  `src/`.
- Keep the module free of `Async`; it must stay usable from a plain test or a client.

# AGENTS.md: cboe-sim

## Purpose

A local simulator of a Cboe US equities and options matching unit. It speaks BOE v2 order
entry and PITCH 2.X market data over TCP, and a JSON websocket feed, on top of the
venue-neutral `exchange-core-engine`. Prices are plausible, not real: nothing here comes
from Cboe.

## Layout

| Path | Holds |
| --- | --- |
| `src/` | The venue: sessions, feed sequencing, clock, instruments, anchoring (see `src/AGENTS.md`) |
| `wire/` | BOE v2, PITCH 2.X, bitfields and the PITCH order book codecs (see `wire/AGENTS.md`) |
| `bin/` | The `cboe-sim` server and the `cboe-sim-cli` client (see `bin/AGENTS.md`) |
| `test/` | Expect tests and Quickcheck properties (see `test/AGENTS.md`) |
| `scripts/` | `e2e.sh` and `anchor-demo.sh` (see `scripts/AGENTS.md`) |
| `.github/` | CI workflow and the shared obj-magic check (see `.github/AGENTS.md`) |
| `README.md` | Flags, ports, fidelity and limitations (user facing) |
| `dune-project` | Package metadata; edit this, not the generated `cboe-sim.opam` |

## Build, test

Run from this repository. The workspace root is one level up, so every command takes
`--root ..`.

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/all
opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/runtest
```

Parallel work with another agent: prefix both with `DUNE_BUILD_DIR=_build-cboe-sim`.

Run the simulator (ports are the README defaults):

```sh
opam exec -- dune exec cboe-sim -- -gateway-port 9300 -feed-port 9301 -http-port 9302 -noise
```

Standalone (outside the workspace): the org opam repository is the target for published
packages (see `../specs/repos/opam-repository.md`). Until a package is there, pin the
siblings from the CI list below in order, then `opam install . --deps-only --with-test`
and `dune build`.

## Conventions

- Format only this repository: `dune build --root .. @cboe-sim/fmt --auto-promote`. Never
  run `dune fmt` at the workspace root; it rewrites every sibling repository.
- Every module has an `.mli`. Libraries are consumed by their public opam names.
- Expect tests with `-source-tree-root .`; promote with `dune promote` only after a
  successful `@all` build, and read the diff.
- Message layouts follow the summaries in `../equities-core/docs/cboe-boe.md` and
  `../equities-core/docs/cboe-pitch.md`. The "Simulator scope" sections there define what
  is implemented; do not add fields the summaries do not list.
- Comments for humans at non-obvious choices (bit offsets, sequencing rules, anchoring).

## Dependencies and dependents

Sibling checkouts used through the workspace: `fixed-decimal`, `exchange-core`
(`exchange-core-engine` package), `order-book`, `equities-core`, `parquet`, `market-data`,
`price-anchor`. CI pins them in that order (`.github/workflows/ci.yml`).

`cboe-sim` is one of the TradFi simulators with `nyse-sim` and `cme-sim`; shared TradFi
semantics (sessions, ticks, halts, fees) live in `../equities-core`.

## CI

`.github/workflows/ci.yml`: checkout, the shared `ox-setup` action (caches the opam switch
keyed on the pins), `dune build`, `dune test`, then the reusable obj-magic workflow from
`../infra`. Pushes cancel the running job, so push once per finished change.

To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
--exit-status`. Never poll in a loop.

## Pitfalls

- A module named `Session` cycles with `Equities_core.Session`; the venue uses `Boe_session`.
- TCP feed writers must set `raise_when_consumer_leaves` to false and watch
  `consumer_left`, or a dropped client raises inside the server.
- The venue clamps every anchor target to the LULD bands (`Price_anchor.Clamp.within_bands`
  in `src/venue.ml`); keep that clamp when changing anchoring.

## Where to read more

- `README.md`: flags, ports, the price anchor, fidelity and limitations, end-to-end run.
- `../specs/repos/cboe-sim.md`: key modules and status.
- `../equities-core/docs/`: the BOE, PITCH and TradFi specification summaries.
- `../oxcaml-claude-skills/skills/oxcaml-performance/` and `../oxcaml-claude-skills/skills/oxcaml-modes/`
  when touching hot paths or mode annotations.
- `../oxcaml-claude-skills/AGENTS.md`: workspace-wide guide.
- `../oxcaml-claude-skills/GOTCHAS.md`: the TradFi simulator section.

## Never do

- Install opam packages that change the compiler or `core`, `base`, `async`, `ppx_jane`
  on the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release.
- Commit credentials or print secrets.
- Poll CI in loops.

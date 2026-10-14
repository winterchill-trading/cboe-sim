# AGENTS.md: cboe-sim/bin

Two executables built with the `cboe-sim` package:

- `cboe-sim` (`main.ml`): the server. Flags are listed in `../README.md` (ports 9300
  gateway, 9301 feed, 9302 HTTP and `/ws`; `-noise`, `-symbols`, anchor flags).
- `cboe-sim-cli` (`cli.ml`): a small client that sends BOE orders and reads PITCH from a running server.

Run the server: `opam exec -- dune exec cboe-sim -- -noise`. Build from the workspace with
`opam exec --switch 5.2.0+ox -- dune build --root .. @cboe-sim/all`.

Rules:
- Parse flags with `Command.async` and `Command_unix.run`. Shutdown work registered with
  `Shutdown.at_shutdown` does not run on SIGINT or SIGTERM by default; add a
  `Signal.handle` for those if the process has state to flush.
- Keep logic out of the binaries; move it into `../src` so tests can reach it.

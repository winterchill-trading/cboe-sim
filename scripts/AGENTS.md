# AGENTS.md: cboe-sim/scripts

- `e2e.sh`: starts the server on high ports (4200 to 4202, overridable with `GW`, `FEED`,
  `HTTP`) with noise traders and an accelerated clock, then drives `cboe-sim-cli` through
  place, fill, modify and cancel and reads the PITCH feed. Everything it starts is stopped
  on exit.
- `anchor-demo.sh`: runs anchored listings (AAPL on a reference series, MSFT fixed) on an
  always-open clock and prints the top of both books from the PITCH feed.

Rules:
- Both scripts `set -euo pipefail` and `cd` to the repository root. Keep that.
- Do not pipe a script into `head`: SIGPIPE stops it before the cleanup runs.
- Scripts must stop the processes they start, including on failure.

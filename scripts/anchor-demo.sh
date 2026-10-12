#!/usr/bin/env bash
# Anchored noise traders: AAPL follows a reference series (230, then 236 after 10 s,
# then 226 after 20 s) and MSFT a fixed 400 (it lists at 420). The clock is always-open,
# so the anchors follow the whole time. Prints the top of both books every few seconds
# from the PITCH feed. Everything started here is stopped on exit.
set -euo pipefail
cd "$(dirname "$0")/.."

GW=${GW:-4210}
FEED=${FEED:-4211}
HTTP=${HTTP:-4212}
HALF_LIFE_MS=${HALF_LIFE_MS:-1000}
OUT=$(mktemp -d)

for p in "$GW" "$FEED" "$HTTP"; do
  if lsof -nP -iTCP:"$p" -sTCP:LISTEN >/dev/null 2>&1; then
    echo "port $p is in use" >&2
    exit 1
  fi
done

# BIN may point at an existing install directory (e.g. a private workspace build).
if [ -z "${BIN:-}" ]; then
  opam exec -- dune build @install 2>&1 | grep -v "directory '" || true
  BIN=${DUNE_BUILD_DIR:-_build}/install/default/bin
fi
SIM=$BIN/cboe-sim
CLI=$BIN/cboe-sim-cli

PIDS=()
cleanup() {
  for pid in ${PIDS[@]+"${PIDS[@]}"}; do kill "$pid" 2>/dev/null || true; done
  wait 2>/dev/null || true
  rm -rf "$OUT"
}
trap cleanup EXIT

# The series' first point is mapped to the simulator's start; only the spacing matters.
cat >"$OUT/aapl.txt" <<SERIES
# time price
2026-10-13T13:30:00Z 230
2026-10-13T13:30:10Z 236
2026-10-13T13:30:20Z 226
SERIES

"$SIM" -gateway-port "$GW" -feed-port "$FEED" -http-port "$HTTP" -seed 7 -no-options \
  -symbols AAPL,MSFT \
  -anchor-series "$OUT/aapl.txt=AAPL" -anchor-fixed 400=MSFT \
  -anchor-half-life-ms "$HALF_LIFE_MS" 2>"$OUT/sim.log" &
PIDS+=($!)
for _ in $(seq 50); do
  lsof -nP -iTCP:"$FEED" -sTCP:LISTEN >/dev/null 2>&1 && break
  sleep 0.1
done
cat "$OUT/sim.log"

top() {
  "$CLI" book -port "$FEED" -symbol "$1" -depth 1 -seconds 0.5 |
    awk -v s="$1" '/ask/ {a=$2} /bid/ {b=$2} END {printf "%-5s bid %9s  ask %9s\n", s, b, a}'
}

start=$(date +%s)
for _ in $(seq 7); do
  printf '\n== t+%ss (AAPL anchor: %s)\n' "$(($(date +%s) - start))" \
    "$(t=$(($(date +%s) - start)); if [ "$t" -lt 10 ]; then echo 230; elif [ "$t" -lt 20 ]; then echo 236; else echo 226; fi)"
  top AAPL
  top MSFT
  sleep 4
done

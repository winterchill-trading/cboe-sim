#!/usr/bin/env bash
# End to end run: start cboe-sim on high ports with noise traders and an accelerated
# clock a few seconds before the open, then use cboe-sim-cli to place, fill, modify and
# cancel orders and to read the PITCH feed. Everything started here is stopped on exit.
set -euo pipefail
cd "$(dirname "$0")/.."

GW=${GW:-4200}
FEED=${FEED:-4201}
HTTP=${HTTP:-4202}
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
  for pid in "${PIDS[@]}"; do kill "$pid" 2>/dev/null || true; done
  wait 2>/dev/null || true
  rm -rf "$OUT"
}
trap cleanup EXIT

step() { printf '\n== %s\n' "$*"; }
boe() { "$CLI" "$1" -port "$GW" "${@:2}" | tee -a "$OUT/boe.txt"; }

step "start cboe-sim (BOE $GW, PITCH $FEED, JSON $HTTP), clock 09:29:54 ET at 2x"
"$SIM" -gateway-port "$GW" -feed-port "$FEED" -http-port "$HTTP" -noise -seed 7 \
  -clock accelerated -clock-start 09:29:54 -clock-speed 2 2>"$OUT/sim.log" &
PIDS+=($!)
for _ in $(seq 50); do
  lsof -nP -iTCP:"$FEED" -sTCP:LISTEN >/dev/null 2>&1 && break
  sleep 0.1
done
cat "$OUT/sim.log"

"$CLI" feed -port "$FEED" -seconds 9 -no-time >"$OUT/feed.txt" &
PIDS+=($!)
curl -s "http://127.0.0.1:$HTTP/" && echo

step "pre-market: on-open orders for the opening auction"
boe order -user OPEN -symbol AAPL -side buy -qty 500 -tif opg -id MOO1 -wait 0.3
boe order -user OPEN -symbol AAPL -side sell -qty 200 -price 229.00 -tif opg -id LOO1 -wait 0.3

step "wait for 09:30 (opening auction)"
sleep 3

step "member AAAA: resting bid, modify, cancel"
boe order -user AAAA -symbol AAPL -side buy -qty 300 -price 225.00 -id BID1 -wait 0.3
boe modify -user AAAA -id BID1 -new-id BID2 -qty 200 -price 225.50 -wait 0.3
boe cancel -user AAAA -id BID2 -wait 0.3

step "member BBBB: marketable IOC sell into the noise traders' bids"
boe order -user BBBB -symbol AAPL -side sell -qty 150 -price 228.00 -tif ioc -id IOC1 -wait 0.3

step "member BBBB on test symbol ZVZZT (no noise): reserve 100 of 300, hidden 50; CCCC takes 200"
boe order -user BBBB -symbol ZVZZT -side sell -qty 300 -price 10.05 -max-floor 100 -id RES1 -wait 0.3
boe order -user BBBB -symbol ZVZZT -side sell -qty 50 -price 10.04 -hidden -id HID1 -wait 0.3
boe order -user CCCC -symbol ZVZZT -side buy -qty 200 -price 10.05 -tif ioc -id TAKE1 -wait 0.3
boe logon -user BBBB -replay -wait 0.3

step "rejects: off-tick price, unknown symbol, post-only that would cross"
boe order -user AAAA -symbol AAPL -side buy -qty 100 -price 229.005 -id BAD1 -wait 0.2
boe order -user AAAA -symbol ZZZZ -side buy -qty 100 -price 10 -id BAD2 -wait 0.2
boe order -user AAAA -symbol ZVZZT -side buy -qty 100 -price 10.05 -post-only -id BAD3 -wait 0.2

step "SPY option by feed symbol (SPY 2026-12-18 600 call)"
boe order -user AAAA -symbol SPY002 -side buy -qty 5 -price 17.50 -id OPT1 -wait 0.3

step "reconnect OPEN with replay: its on-open orders were filled in the auction"
boe logon -user OPEN -replay -wait 0.2

step "AAPL book from the feed"
"$CLI" book -port "$FEED" -symbol AAPL -seconds 0.5 -depth 3

wait "${PIDS[1]}" 2>/dev/null || true
step "PITCH feed excerpt (definitions, spin, auction, our orders)"
ids=$(grep -oE '(BID1|RES1) order_id=[0-9]+' "$OUT/boe.txt" | cut -d= -f2 | sort -u | paste -sd'|' -)
grep -E "SymbolMapping SPY002|TradingStatus (AAPL|ZVZZT)|SpinResponse|Auction|ZVZZT|Trade id=0 |id=($ids)( |$)" "$OUT/feed.txt" | head -40 || true
echo "... $(wc -l <"$OUT/feed.txt") feed lines in total"

step "stop"
kill "${PIDS[0]}"
wait "${PIDS[0]}" 2>/dev/null || true
for p in "$GW" "$FEED" "$HTTP"; do
  if lsof -nP -iTCP:"$p" -sTCP:LISTEN >/dev/null 2>&1; then echo "port $p still open" >&2; fi
done
echo "done"

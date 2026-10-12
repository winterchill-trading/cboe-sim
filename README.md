# cboe-sim

A free, local stand-in for a Cboe US equities exchange (BZX style) with a small SPY
options section. It speaks the venue's real public binary protocols over TCP:

- **BOE v2** (Binary Order Entry) on the gateway port: login with unit sequences and
  return bitfields, replay on reconnect, heartbeats, New Order, Cancel Order, Modify Order
  and the acknowledgement, reject, cancel, execution and restatement messages;
- **PITCH 2.X** on the feed port: Sequenced Unit Header frames carried over a plain TCP
  stream (the framing Cboe uses for its gap and spin servers), one message per frame, a
  spin of the displayed book on connect, then the live stream;
- the same feed events as JSON objects on `ws://host:<http-port>/ws`, so a market data
  client can use them without binary parsing.

**There is no real-time data for this venue.** Nothing here comes from Cboe: the books are
driven by noise traders from `exchange-core-engine`, optionally anchored to a reference
price you choose (see [Price anchor](#price-anchor)). Prices are plausible, not real.

Message layouts follow the public specifications summarised in
`equities-core/docs/cboe-boe.md` and `equities-core/docs/cboe-pitch.md` (Cboe Titanium
U.S. Equities BOE v2.4.58 and Multicast PITCH v2.41.82); the "Simulator scope" sections of
those summaries define what is implemented.

## Build and run

```sh
opam exec -- dune build
opam exec -- dune exec cboe-sim -- -gateway-port 9300 -feed-port 9301 -http-port 9302 -noise
```

Flags:

| Flag | Default | Meaning |
| --- | --- | --- |
| `-gateway-port` | 9300 | BOE order entry (TCP) |
| `-feed-port` | 9301 | PITCH feed (TCP) |
| `-http-port` | 9302 | status JSON on `/`, JSON feed websocket on `/ws` |
| `-symbols AAPL,MSFT` | all | equities to list: AAPL, MSFT, NVDA, AMZN, SPY, ZVZZT |
| `-no-options` | | do not list the SPY options |
| `-noise` | off | run noise traders (not on the test symbol ZVZZT) |
| `-noise-traders N`, `-seed N` | 6, 0 | noise traders per listing, random seed |
| `-clock` | `always-open` | `real` (New York time, NYSE calendar), `accelerated`, `always-open` |
| `-clock-start`, `-clock-date`, `-clock-speed` | 09:25:00, today, 60 | accelerated clock: virtual start time and speed |
| `-anchor-series FILE=SYM` | | anchor SYM to a series of `<time> <price>` lines (repeatable) |
| `-anchor-fixed PRICE[,DRIFT]=SYM` | | anchor SYM to a fixed price, with a compounded drift in bps per day (repeatable) |
| `-anchor-scaled VENUE:SYMBOL=SYM,FACTOR` | | anchor SYM to a recorded reference times FACTOR, from `-anchor-norm DIR -anchor-from DATE [-anchor-to DATE]` (repeatable) |
| `-anchor-half-life-ms` | 1000 | half-life of the pull towards the anchor |

`always-open` stays in the core session (no auctions), which is what you want to play.
`accelerated` replays a whole day: pre-market, opening auction at 09:30, core session,
closing auction at 16:00, post-market until 20:00, end of session; at speed 60 a day takes
about 16 minutes.

Listings: equities on matching unit 1 with Tier 1 LULD bands (ZVZZT Tier 2); SPY options
expiring 2026-12-18 at strikes 590/600/610, calls `SPY001`-`SPY003` and puts
`SPY004`-`SPY006`, on unit 2 (penny ticks, contracts, core hours only).

CI installs the pinned siblings and the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.

## Price anchor

There is no real-time Cboe data to shadow, so an anchor does not place reference levels
or replay reference trades (the crypto simulators do). It pulls a listing's noise traders
towards a reference price instead: each 100 ms tick the noise walk moves by its random
step plus `k * (anchor - walk)` with `k = 1 - 0.5 ** (tick / half_life)`
(`Market_sim.set_reference ~half_life`), so the book, the trades and the mark follow the
reference with noise around it.

`Anchoring` is a thin wrapper over `price-anchor`'s `Anchor`: one per anchored listing,
created with the listing's noise trader count, `half_life` (`-anchor-half-life-ms`,
default 1 s), `step_clock:false` (the venue owns its clock), `levels:0`, a zero
`mark_interval`, and a `Path.t` pulled on the exchange's clock at every tick. The
reference is one of:

- a series file (`-anchor-series`): `<time> <price>` per line, the time in nanoseconds
  since the epoch or ISO 8601 with a zone, `#` comments (`Source.of_series`, then
  `Source.to_path`). The first point is mapped to the simulator's start; the price is a
  step function of the points and holds the last one after the end;
- a fixed price with an optional drift (`-anchor-fixed 250,20=AAPL`: 250, +20 bps per day
  of simulator time; `Path.fixed_of_string`);
- a recorded crypto reference scaled into the listing's range
  (`-anchor-scaled binance:BTCUSDT=SPY,0.006 -anchor-norm data/norm -anchor-from
  2026-10-01`): the recorder's normalized event logs (`Source.of_norm`, then
  `Source.to_path ~min_interval:1s`), the book mid (L2 or BBO) else the last trade, a
  point kept when the price changed at least a second after the last one. The factor is
  the anchor's `price_factor`, which rounds to the listing's tick.

The venue hands the anchor three callbacks:

- `may_engage`: continuous core trading only. In the auction windows (five minutes
  before the open and the close), during LULD pauses and outside core hours the venue
  owns the price: the anchor gives the symbol back to the free random walk (a pause,
  counted once per stretch) and waits.
- `last_trade`: the venue's last print. When the anchor follows again (the open, the end
  of a pause) the walk restarts from it and climbs back towards the reference.
- `clamp`: `Clamp.within_bands` on the listing's LULD bands (from this venue's own
  prints, the mean over the last five minutes), 30 bps inside them. A reference that
  jumps further than the band moves the target only as far as the bands allow, and the
  walk climbs with the bands as the prints move them, instead of tripping a limit state.

Changes from the module this replaced (expect tests updated accordingly): `waits`
counts the pause before the first follow too (it now counts pauses, including the
initial one); `resumes` counts follows after a pause only; the walk restarts from the
last print on resume; targets are clamped to the bands; recorded references keep the
first change per second (book mids over trades) instead of the last price; scaled prices
round to the tick. Anchors turn `-noise` on; the test symbol ZVZZT has no noise traders
and cannot be anchored.

`scripts/anchor-demo.sh` runs AAPL on a series (230, 236 after 10 s, 226 after 20 s)
and MSFT on a fixed 400 (it lists at 420), and prints the top of both books every five
seconds. One run (2026-10-12, always-open clock, half-life 1 s):

```text
== t+0s (AAPL anchor: 230)
AAPL  bid    229.94  ask    230.05
MSFT  bid    411.72  ask    417.79

== t+6s (AAPL anchor: 230)
AAPL  bid    229.96  ask    230.04
MSFT  bid    400.23  ask    400.35

== t+16s (AAPL anchor: 236)
AAPL  bid    235.59  ask    235.74
MSFT  bid    399.77  ask    400.09

== t+26s (AAPL anchor: 226)
AAPL  bid    226.22  ask    226.31
MSFT  bid    399.96  ask    399.99

== t+31s (AAPL anchor: 226)
AAPL  bid    225.91  ask    225.94
MSFT  bid    399.76  ask    399.79
```

Snapshots taken right after a step (t+11s, t+21s) show the book still in transit.

Raw minute files are not a source here: their normalizers live in the venue client and
feed repositories, and linking them all into a TradFi simulator is not worth it. Rebuild
`norm/` with the recorder (`market-recorder rebuild-norm`) and use `-anchor-scaled`.

## Play with the CLI

```sh
CLI="dune exec cboe-sim-cli --"
$CLI feed -port 9301 -seconds 5 -no-time        # decoded PITCH: definitions, spin, live
$CLI book -port 9301 -symbol AAPL               # book rebuilt from the feed
$CLI order -port 9300 -user TEST -symbol AAPL -side buy -qty 100 -price 229.50
$CLI order -port 9300 -user TEST -symbol AAPL -side sell -qty 100 -tif ioc -price 228
$CLI order -port 9300 -user TEST -symbol ZVZZT -side sell -qty 300 -price 10.05 -max-floor 100
$CLI modify -port 9300 -user TEST -id C1 -new-id C2 -price 229.40
$CLI cancel -port 9300 -user TEST -id C2
$CLI logon -port 9300 -user TEST -replay        # replay every sequenced message
$CLI order -port 9300 -symbol SPY002 -side buy -qty 5 -price 17.50   # an option
```

Any username and password log in; the session is username plus session sub id (`-sub`,
default `0001`), and orders, sequence numbers and the outbound message log survive
reconnects. Other order flags: `-tif day|gtc|ioc|fok|opg|clo`, no `-price` for a market
order, `-hidden`, `-post-only`, `-stp NF|OF|BF`, `-side short|short-exempt`.

The binary codecs are a separate library, `cboe-sim.wire` (`Boe`, `Pitch`, `Bitfields`,
`Pitch_book`, `Pretty`), for writing other clients.

## What is simulated (fidelity vs the spec summaries)

| Area | Spec | Here |
| --- | --- | --- |
| BOE framing | `0xBABA`, length, type, unit, sequence | as specified |
| Login | SessionSubID, Username, Password, param groups `0x80` unit sequences, `0x81` return bitfields | as specified; any credentials accepted; status `A`, `N`, `B`, `F` |
| Login Response | status, text, last received sequence, per-unit sequences, echoed groups | as specified |
| Replay | missed outbound messages per unit, then Replay Complete | as specified; replay from the per-unit sequences sent, all units when `NoUnspecifiedUnitReplay` is 0 |
| Heartbeats | each side after 1 s of silence | server heartbeats after 1 s; logout (`!`) after 5 s without inbound traffic |
| Sequencing | inbound one stream, outbound per unit | as specified; inbound sequences already processed are ignored (resends), gaps accepted |
| New Order | ClOrdID, Side 1/2/5/6, OrderQty, bitfield fields | Price, OrdType 1/2, TimeInForce 0/1/2/3/4/7, Symbol, Capacity (required), RoutingInst `B`/`P` (`R` treated as `B`), MaxFloor, DisplayIndicator `I`, PreventMatch, ExecInst `f`, options MaturityDate/StrikePrice/PutOrCall; other known fields accepted and ignored |
| Cancel / Modify | OrigClOrdID; Modify price and size | as specified; side changes rejected |
| Outbound messages | Ack, Rejected, Modified, Restated, User Modify Rejected, Cancelled, Cancel Rejected, Execution | all, with the offsets of the summary; rejects unsequenced |
| Return bitfields | per message type chosen at login | bytes 1-3 per the summary (verified range), bytes 4-5 from the v2 layout (not verified); Side, Price, Symbol, OrderQty, LeavesQty, LastShares, LastPx, OrigClOrdID and more filled in |
| Reason codes | `A D H J M N O S U V W X Y Z u` | `A` admin/validation, `D` duplicate, `H` LULD pause, `J` too late, `M` size, `N` IOC/FOK unfilled, `O` unknown id, `S` Rule 201, `U` user, `V` STP, `W` post-only, `X` expired, `Y` symbol, `Z` other, `u` LULD band |
| Exec ids | 9 base-36 chars, `C` prefix for auctions | as specified |
| Sessions | pre-market, core, post-market (equities-core `Session`) | phases enforced: market orders core only, options core only, closed market rejects, Day orders expire after the close, everything expires at 20:00 |
| Auctions | opening and closing, Auction Update / Summary | on-open (TIF 2) and on-close (TIF 7) orders uncross with the book at 09:30 / 16:00 (equities-core `Auction`); Auction Update every second during the 5 minutes before |
| LULD | bands, limit state, 5 min pause | Tier 1/2 bands against the mark; orders outside rejected `u`; limit state and pause per equities-core `Halts` with Trading Status `H` |
| Rule 201 | short sale price test | short sales at or below the best bid rejected `S` once triggered; Trading Status Reg SHO flag |
| PITCH framing | Sequenced Unit Header, heartbeats (count 0) | as specified; one message per frame |
| PITCH messages | Time, Add Order, Order Executed (at price), Reduce Size, Modify, Delete, Trade, Trading Status, Auction Update/Summary, End of Session, Symbol Mapping | all of these, long variants; options unit messages carry the trade condition byte |
| Spin | Spin Response, Add Orders, Spin Finished | sent unsequenced on connect, after Time, Symbol Mapping and Trading Status |
| Hidden / reserve | hidden fills as Trade with obfuscated id; reserve reload with a new order id | as specified |

## Not simulated / limitations

- No multicast, A/B feeds, gap request proxy or spin server ports; the feed is one TCP
  stream with the spin at the start. No Time Reference, RPI, width or options auction
  messages; no short or expanded PITCH variants are sent.
- No routing, pegs, discretion, stops, purge or risk messages, trade cancel/correct,
  options crosses and quotes, BOE v3.
- The engine always prevents self-matches (cancel newest). PreventMatch `O` and `B` are
  done on top; `D` and `S` behave like `N`. Without PreventMatch Cboe would let a
  member trade with itself, the simulator does not.
- A modify re-queues the order in the engine (loss of priority even for size reductions),
  although the BOE and PITCH order ids stay the same (the feed sends Reduce Size for a
  size-only decrease, as Cboe would).
- Reserve orders: only the displayed slice is in the engine, so a single incoming order
  cannot take more than the slice; the hidden reserve reloads after the slice fills.
- Auction executions of book orders cancel and re-place the remainder in the engine; they
  are not settled in the engine's clearinghouse. Auction-only executions appear only in
  the Auction Summary, not as Trade messages. No halt reopening auction after a LULD pause.
- LULD reference price is the mean of this venue's prints over the last five minutes (the
  mark when there were none), recomputed on every check rather than only on 1% moves;
  there is no SIP, so "national" best bid and offer are this book's.
- Options trade on their own random walk, not linked to SPY; no option LULD or auctions.
- Fees use the equities-core BZX-like schedule inside the engine but are not reported.
- Return bitfields beyond bytes 1-3 and the Logout / Order Modified layouts are not
  verified against Cboe (see the spec summaries).

## End to end

`scripts/e2e.sh` starts the simulator on ports 4200-4202 with noise traders and an
accelerated clock a few seconds before the open, then uses the CLI: on-open orders filled
in the opening auction, a resting bid modified and cancelled, an IOC sell filled against
the noise traders, a reserve and a hidden order on the test symbol, rejects, an option
order, a reconnect with replay, the book and a feed excerpt. It stops everything it
started. Output of one run (login and logout lines removed; prices vary run to run):

```text

== start cboe-sim (BOE 4200, PITCH 4201, JSON 4202), clock 09:29:54 ET at 2x

== pre-market: on-open orders for the opening auction
> NewOrder MOO1 side=1 qty=500 sym=AAPL
< [u1 #1] OrderAck MOO1 order_id=1000000000 sym=AAPL side=1 qty=500 leaves=500
> NewOrder LOO1 side=2 qty=200 sym=AAPL px=229
< [u1 #2] OrderAck LOO1 order_id=1000000001 sym=AAPL side=2 px=229 qty=200 leaves=200

== wait for 09:30 (opening auction)

== member AAAA: resting bid, modify, cancel
> NewOrder BID1 side=1 qty=300 sym=AAPL px=225
< [u1 #1] OrderAck BID1 order_id=604 sym=AAPL side=1 px=225 qty=300 leaves=300
> ModifyOrder BID2 orig=BID1 qty=200 px=225.5
< [u1 #2] OrderModified BID2 order_id=604 sym=AAPL side=1 px=225.5 qty=200 leaves=200 orig=BID1
> CancelOrder BID2
< [u1 #3] OrderCancelled BID2 reason=U sym=AAPL side=1 px=225.5 qty=200 leaves=200 orig=BID1

== member BBBB: marketable IOC sell into the noise traders' bids
> NewOrder IOC1 side=2 qty=150 sym=AAPL px=228
< [u1 #1] OrderAck IOC1 order_id=820 sym=AAPL side=2 px=228 qty=150 leaves=150
< [u1 #2] OrderExecution IOC1 exec=00000002C 24@230.12 leaves=126 liq=R
< [u1 #3] OrderExecution IOC1 exec=00000002D 126@230.09 leaves=0 liq=R

== member BBBB on test symbol ZVZZT (no noise): reserve 100 of 300, hidden 50; CCCC takes 200
> NewOrder RES1 side=2 qty=300 sym=ZVZZT px=10.05
< [u1 #4] OrderAck RES1 order_id=892 sym=ZVZZT side=2 px=10.05 qty=300 leaves=300
> NewOrder HID1 side=2 qty=50 sym=ZVZZT px=10.04
< [u1 #5] OrderAck HID1 order_id=945 sym=ZVZZT side=2 px=10.04 qty=50 leaves=50
> NewOrder TAKE1 side=1 qty=200 sym=ZVZZT px=10.05
< [u1 #1] OrderAck TAKE1 order_id=1011 sym=ZVZZT side=1 px=10.05 qty=200 leaves=200
< [u1 #2] OrderExecution TAKE1 exec=000000030 50@10.04 leaves=150 liq=R
< [u1 #3] OrderExecution TAKE1 exec=000000031 100@10.05 leaves=50 liq=R
< [u1 #4] OrderCancelled TAKE1 reason=N sym=ZVZZT side=1 px=10.05 qty=200 leaves=50
< [u1 #1] OrderAck IOC1 order_id=820 sym=AAPL side=2 px=228 qty=150 leaves=150
< [u1 #2] OrderExecution IOC1 exec=00000002C 24@230.12 leaves=126 liq=R
< [u1 #3] OrderExecution IOC1 exec=00000002D 126@230.09 leaves=0 liq=R
< [u1 #4] OrderAck RES1 order_id=892 sym=ZVZZT side=2 px=10.05 qty=300 leaves=300
< [u1 #5] OrderAck HID1 order_id=945 sym=ZVZZT side=2 px=10.04 qty=50 leaves=50
< [u1 #6] OrderExecution HID1 exec=000000030 50@10.04 leaves=0 liq=A
< [u1 #7] OrderExecution RES1 exec=000000031 100@10.05 leaves=200 liq=A
< [u1 #8] OrderRestated RES1 order_id=1012 reason=R sym=ZVZZT side=2 px=10.05 qty=300 leaves=200

== rejects: off-tick price, unknown symbol, post-only that would cross
> NewOrder BAD1 side=1 qty=100 sym=AAPL px=229.005
< OrderRejected BAD1 reason=A "Price not on the tick"
> NewOrder BAD2 side=1 qty=100 sym=ZZZZ px=10
< OrderRejected BAD2 reason=Y "symbol not supported"
> NewOrder BAD3 side=1 qty=100 sym=ZVZZT px=10.05
< OrderRejected BAD3 reason=W "post only order would remove liquidity"

== SPY option by feed symbol (SPY 2026-12-18 600 call)
> NewOrder OPT1 side=1 qty=5 sym=SPY002 px=17.5
< [u2 #1] OrderAck OPT1 order_id=1266 sym=SPY002 side=1 px=17.5 qty=5 leaves=5

== reconnect OPEN with replay: its on-open orders were filled in the auction
< [u1 #1] OrderAck MOO1 order_id=1000000000 sym=AAPL side=1 qty=500 leaves=500
< [u1 #2] OrderAck LOO1 order_id=1000000001 sym=AAPL side=2 px=229 qty=200 leaves=200
< [u1 #3] OrderExecution MOO1 exec=C00000001 500@230.01 leaves=0 liq=C
< [u1 #4] OrderExecution LOO1 exec=C00000002 200@230.01 leaves=0 liq=C

== AAPL book from the feed
AAPL book
  ask     230.14      186 (2)
  ask     230.13       13 (1)
  ask     230.12      130 (1)
  ----
  bid     229.95       11 (1)
  bid     229.94       17 (1)
  bid     229.92      165 (2)

== PITCH feed excerpt (definitions, spin, auction, our orders)
[u1 spin] TradingStatus AAPL T regsho=0
[u1 spin] TradingStatus ZVZZT T regsho=0
[u1 spin] SpinResponse seq=103 orders=62 status=A
[u2 spin] SymbolMapping SPY002 -> "SPY   261218C00600000" (SPY)
[u2 spin] SpinResponse seq=2 orders=0 status=A
[u1 #127] AuctionUpdate AAPL O ref=229.97 buy=500 sell=0 indicative=230.26 auction_only=0
[u1 #275] AuctionUpdate AAPL O ref=230 buy=500 sell=309 indicative=230.14 auction_only=230
[u1 #412] AuctionSummary AAPL O 500@230.01
[u1 #686] AddOrder id=604 AAPL B 300@225
[u1 #755] ModifyOrder id=604 200@225.5
[u1 #808] DeleteOrder id=604
[u1 #932] AddOrder id=892 ZVZZT S 100@10.05
[u1 #1031] Trade id=0 ZVZZT 50@10.04 exec=000000030
[u1 #1032] OrderExecuted id=892 qty=100 exec=000000031
[u1 #1033] AddOrder id=1012 ZVZZT S 100@10.05
... 2818 feed lines in total

== stop
done
```

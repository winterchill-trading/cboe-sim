open! Core
open Cboe_sim
module E = Exchange_core
module Market_sim = Exchange_core_engine.Market_sim
module Exchange = Exchange_core_engine.Exchange

let day = Date.create_exn ~y:2026 ~m:Oct ~d:13

let venue ~at target =
  let clock = Clock.manual (Clock.ny_time day at) in
  let v =
    Venue.create
      ~config:{ Venue.Config.default with noise = true; anchors = [ "AAPL", target ] }
      ~clock
      ~emit_user:(fun ~user:_ ~unit:_ _ -> ())
      ~emit_feed:(fun ~unit:_ _ -> ())
      (Instruments.universe ~symbols:[ "AAPL" ] ~options:false ())
  in
  v, clock
;;

let price = Option.value_map ~default:"-" ~f:E.Price.to_string

let show v clock =
  let anchor = Venue.anchor v "AAPL" |> Option.value_exn in
  let sim = Venue.market_sim v "AAPL" |> Option.value_exn in
  let _, tod = Clock.local clock in
  printf
    !"%s %{sexp:Equities_core.Session.Us_equities.Phase.t} %{sexp:Anchoring.State.t} \
      resumes=%d waits=%d walk=%s mark=%s luld_ref=%s\n"
    (Time_ns.Ofday.to_millisecond_string
       (Time_ns.Ofday.of_span_since_start_of_day_exn tod))
    (Clock.phase clock)
    (Anchoring.state anchor)
    (Anchoring.resumes anchor)
    (Anchoring.waits anchor)
    (price (Market_sim.reference_price sim (E.Symbol.of_string "AAPL")))
    (price (Exchange.mark (Venue.exchange v) (E.Symbol.of_string "AAPL")))
    (price (Venue.luld_reference v "AAPL"))
;;

(* Ticks every 100 ms of venue time up to [until] (New York time). *)
let run_until v clock until =
  let until = Clock.ny_time day until in
  while Time_ns.( < ) (Clock.now clock) until do
    Clock.set clock (Time_ns.add (Clock.now clock) (Time_ns.Span.of_int_ms 100));
    Venue.tick v
  done
;;

let tick_n v clock n =
  for _ = 1 to n do
    Clock.set clock (Time_ns.add (Clock.now clock) (Time_ns.Span.of_int_ms 100));
    Venue.tick v
  done
;;

let fixed px = Anchoring.Target.fixed (E.Price.of_string px) |> ok_exn

let%expect_test "the anchor waits in the auction windows and resumes at the open" =
  let v, clock = venue ~at:"09:27:00" (fixed "232.30") in
  Venue.tick v;
  show v clock;
  run_until v clock "09:29:59.9";
  show v clock;
  run_until v clock "09:30:00";
  show v clock;
  run_until v clock "09:32:00";
  show v clock;
  Clock.set clock (Clock.ny_time day "15:55:00");
  Venue.tick v;
  show v clock;
  run_until v clock "16:00:00";
  show v clock;
  [%expect
    {|
    09:27:00.000 Pre_market Waiting resumes=0 waits=0 walk=230.05 mark=230.05 luld_ref=230.05
    09:29:59.900 Pre_market Waiting resumes=0 waits=0 walk=228.63 mark=228.63 luld_ref=229.38845588
    09:30:00.000 Core Following resumes=1 waits=0 walk=228.89 mark=228.89 luld_ref=229.38845588
    09:32:00.000 Core Following resumes=1 waits=0 walk=232.18 mark=232.18 luld_ref=230.39564564
    15:55:00.000 Core Waiting resumes=1 waits=1 walk=232.16 mark=232.16 luld_ref=232.2
    16:00:00.000 Post_market Waiting resumes=1 waits=1 walk=232.62 mark=232.62 luld_ref=233.11339421
    |}]
;;

let%expect_test "LULD bands follow the venue's own prints, and a pause stops the anchor" =
  (* 230 for two minutes, then a jump to 260 (13%, beyond the 5% band). *)
  let target =
    let t0 = Time_ns.epoch in
    Anchoring.Target.series
      [ t0, E.Price.of_string "230"
      ; Time_ns.add t0 (Time_ns.Span.of_min 2.), E.Price.of_string "260"
      ]
    |> ok_exn
  in
  let v, clock = venue ~at:"10:00:00" target in
  let anchor = Venue.anchor v "AAPL" |> Option.value_exn in
  Venue.tick v;
  tick_n v clock 1_199;
  show v clock;
  let last = ref (Anchoring.state anchor, Anchoring.waits anchor) in
  for _ = 1 to 6_000 do
    tick_n v clock 1;
    let now = Anchoring.state anchor, Anchoring.waits anchor in
    if not ([%equal: Anchoring.State.t * int] now !last)
    then (
      last := now;
      show v clock)
  done;
  show v clock;
  [%expect
    {|
    10:01:59.900 Core Following resumes=1 waits=0 walk=230.01 mark=230.01 luld_ref=229.9777459
    10:02:18.700 Core Waiting resumes=1 waits=1 walk=259.95 mark=259.95 luld_ref=236.59773841
    10:07:18.700 Core Following resumes=2 waits=1 walk=259.94 mark=259.94 luld_ref=259.94
    10:11:59.900 Core Following resumes=2 waits=1 walk=260.13 mark=260.13 luld_ref=260.00191094
    |}]
;;

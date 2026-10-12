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
      resumes=%d waits=%d clamped=%d walk=%s mark=%s luld_ref=%s\n"
    (Time_ns.Ofday.to_millisecond_string
       (Time_ns.Ofday.of_span_since_start_of_day_exn tod))
    (Clock.phase clock)
    (Anchoring.state anchor)
    (Anchoring.resumes anchor)
    (Anchoring.waits anchor)
    (Anchoring.stats anchor).clamped
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
    09:27:00.000 Pre_market Waiting resumes=0 waits=1 clamped=0 walk=230.05 mark=230.05 luld_ref=230.05
    09:29:59.900 Pre_market Waiting resumes=0 waits=1 clamped=0 walk=228.63 mark=228.63 luld_ref=229.38845588
    09:30:00.000 Core Following resumes=1 waits=1 clamped=0 walk=228.88 mark=228.88 luld_ref=229.38845588
    09:32:00.000 Core Following resumes=1 waits=1 clamped=0 walk=232.18 mark=232.18 luld_ref=230.3956006
    15:55:00.000 Core Waiting resumes=1 waits=2 clamped=0 walk=232.16 mark=232.16 luld_ref=232.2
    16:00:00.000 Post_market Waiting resumes=1 waits=2 clamped=0 walk=232.62 mark=232.62 luld_ref=233.11339421
    |}]
;;

let%expect_test "the target stays inside the LULD bands, which follow the venue's prints" =
  (* 230 for two minutes, then a jump to 260 (13%, beyond the 5% band). The clamp keeps
     the target 30 bps inside the bands, so the walk climbs with the bands instead of
     running into a LULD pause. *)
  let target =
    let t0 = Time_ns.epoch in
    Anchoring.Target.series
      [ t0, E.Price.of_string "230"
      ; Time_ns.add t0 (Time_ns.Span.of_min 2.), E.Price.of_string "260"
      ]
    |> ok_exn
  in
  let v, clock = venue ~at:"10:00:00" target in
  Venue.tick v;
  tick_n v clock 1_199;
  show v clock;
  for _ = 1 to 10 do
    tick_n v clock 600;
    show v clock
  done;
  [%expect
    {|
    10:01:59.900 Core Following resumes=0 waits=0 clamped=0 walk=230.01 mark=230.01 luld_ref=229.9777459
    10:02:59.900 Core Following resumes=0 waits=0 clamped=122 walk=246.59 mark=246.59 luld_ref=235.5094824
    10:03:59.900 Core Following resumes=0 waits=0 clamped=184 walk=248.72 mark=248.72 luld_ref=237.7243824
    10:04:59.900 Core Following resumes=0 waits=0 clamped=264 walk=251.04 mark=251.04 luld_ref=239.88026425
    10:05:59.900 Core Following resumes=0 waits=0 clamped=395 walk=254.57 mark=254.57 luld_ref=243.26612516
    10:06:59.900 Core Following resumes=0 waits=0 clamped=548 walk=260.16 mark=260.16 luld_ref=249.06705276
    10:07:59.900 Core Following resumes=0 waits=0 clamped=548 walk=260.04 mark=260.04 luld_ref=253.81880804
    10:08:59.900 Core Following resumes=0 waits=0 clamped=548 walk=260.09 mark=260.09 luld_ref=255.98449319
    10:09:59.900 Core Following resumes=0 waits=0 clamped=548 walk=259.97 mark=259.97 luld_ref=257.85364069
    10:10:59.900 Core Following resumes=0 waits=0 clamped=548 walk=259.99 mark=259.99 luld_ref=259.26516129
    10:11:59.900 Core Following resumes=0 waits=0 clamped=548 walk=259.83 mark=259.83 luld_ref=260.01225641
    |}]
;;

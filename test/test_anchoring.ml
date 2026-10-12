open! Core
open Cboe_sim
module E = Exchange_core
module Market_sim = Exchange_core_engine.Market_sim

let t0 = Time_ns.of_string_with_utc_offset "2026-10-13T14:00:00Z"
let px = E.Price.of_string
let elapsed s = Time_ns.Span.of_sec s

let print_prices target spans =
  List.iter spans ~f:(fun s ->
    printf
      "%8.0fs %s\n"
      s
      (E.Price.to_string (Anchoring.Target.price_at target ~elapsed:(elapsed s))))
;;

let%expect_test "fixed price with a compounded daily drift" =
  let target = Anchoring.Target.parse_fixed "200,100" |> ok_exn in
  print_prices target [ 0.; 43_200.; 86_400.; 172_800. ];
  [%expect
    {|
           0s 200
       43200s 200.99751242
       86400s 202
      172800s 204.02
    |}];
  print_s [%sexp (Anchoring.Target.parse_fixed "200" |> ok_exn : Anchoring.Target.t)];
  [%expect {| (Fixed (price 200) (drift_bps_per_day 0)) |}];
  List.iter [ "0"; "x"; "1,2,3" ] ~f:(fun s ->
    print_s [%sexp (Result.is_error (Anchoring.Target.parse_fixed s) : bool)]);
  [%expect {|
    true
    true
    true
    |}]
;;

let%expect_test "series: a step function from its first point, last price held" =
  let target =
    Anchoring.Target.series
      [ Time_ns.add t0 (elapsed 10.), px "101"
      ; t0, px "100"
      ; Time_ns.add t0 (elapsed 20.), px "99.5"
      ]
    |> ok_exn
  in
  print_prices target [ 0.; 9.; 10.; 15.; 20.; 3600. ];
  [%expect
    {|
           0s 100
           9s 100
          10s 101
          15s 101
          20s 99.5
        3600s 99.5
    |}];
  print_s [%sexp (Result.is_error (Anchoring.Target.series []) : bool)];
  [%expect {| true |}]
;;

let%expect_test "series file through price-anchor's parser, scaled" =
  Async.Thread_safe.block_on_async_exn (fun () ->
    let open Async in
    let file = Filename_unix.temp_file "series" ".txt" in
    let%bind () =
      Writer.save
        file
        ~contents:"# SPX proxy\n2026-10-13T14:00:00Z 6500\n\n2026-10-13T14:00:05Z 6510\n"
    in
    let%map target = Anchoring.Target.load_series ~factor:0.1 file in
    let target = ok_exn target in
    print_prices target [ 0.; 5. ];
    [%expect {|
             0s 650
             5s 651
      |}])
;;

let%expect_test "update: follows, waits once, resumes" =
  let symbol = E.Symbol.of_string "AAPL" in
  let exchange =
    Exchange_core_engine.Exchange.create
      ~config:
        { Exchange_core_engine.Exchange.Config.default with
          instruments =
            [ { (List.hd_exn Exchange_core_engine.Exchange.Config.default.instruments) with
                symbol
              }
            ]
        }
      ()
  in
  let sim =
    Market_sim.create
      { Market_sim.Config.default with
        symbols = [ symbol ]
      ; initial_prices = [ symbol, px "100" ]
      ; vol_bps = 0.
      }
      exchange
  in
  let a =
    Anchoring.create
      ~half_life:(Time_ns.Span.of_sec 0.1)
      (Anchoring.Target.fixed (px "120") |> ok_exn)
  in
  let show label =
    printf
      !"%s: %{sexp:Anchoring.State.t} walk=%s resumes=%d waits=%d\n"
      label
      (Anchoring.state a)
      (Option.value_map
         (Market_sim.reference_price sim symbol)
         ~default:"-"
         ~f:E.Price.to_string)
      (Anchoring.resumes a)
      (Anchoring.waits a)
  in
  let run ~follow n =
    for _ = 1 to n do
      Anchoring.update a ~now:t0 ~follow ~sim ~symbol;
      Market_sim.tick sim
    done
  in
  show "start";
  run ~follow:false 3;
  show "auction";
  run ~follow:true 1;
  show "open, one half-life";
  run ~follow:true 9;
  show "open, ten half-lives";
  run ~follow:false 5;
  show "pause";
  run ~follow:true 1;
  show "resumed";
  [%expect
    {|
    start: Waiting walk=100 resumes=0 waits=0
    auction: Waiting walk=100 resumes=0 waits=0
    open, one half-life: Following walk=110 resumes=1 waits=0
    open, ten half-lives: Following walk=120 resumes=1 waits=0
    pause: Waiting walk=120 resumes=1 waits=1
    resumed: Following walk=120 resumes=2 waits=1
    |}]
;;

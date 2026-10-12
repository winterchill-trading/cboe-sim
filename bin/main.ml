open! Core
open! Async
open Cboe_sim

let clock_arg =
  Command.Arg_type.of_alist_exn
    [ "real", `Real; "accelerated", `Accelerated; "always-open", `Always_open ]
;;

(* Today if it is a trading day, else the next one. *)
let trading_date () =
  let today = Date.today ~zone:Time_float.Zone.utc in
  if Equities_core.Session.Us_equities.is_trading_day today
  then today
  else Equities_core.Session.Us_equities.next_trading_day today
;;

(* [VALUE=SYMBOL]: the simulator symbol after the last [=]. *)
let assignment ~flag s =
  match String.rsplit2 s ~on:'=' with
  | Some (value, symbol) when not (String.is_empty value || String.is_empty symbol) ->
    value, symbol
  | _ ->
    Core.eprintf "%s: expected VALUE=SYMBOL, got %S\n" flag s;
    Stdlib.exit 2
;;

let or_exit = function
  | Ok x -> x
  | Error e ->
    Core.eprintf "%s\n" (Error.to_string_hum e);
    Stdlib.exit 2
;;

(* [VENUE:SYMBOL=SIM,FACTOR] *)
let parse_scaled s =
  let reference, rest = assignment ~flag:"-anchor-scaled" s in
  match String.lsplit2 rest ~on:',' with
  | Some (symbol, factor) ->
    (match Float.of_string_opt factor with
     | Some factor -> Market_data.Instrument.of_string reference, symbol, factor
     | None ->
       Core.eprintf "-anchor-scaled: bad factor %S\n" factor;
       Stdlib.exit 2)
  | None ->
    Core.eprintf "-anchor-scaled: expected VENUE:SYMBOL=SIM,FACTOR, got %S\n" s;
    Stdlib.exit 2
;;

let load_scaled ~norm ~from ~to_ scaled =
  match scaled, norm with
  | [], _ -> return []
  | _ :: _, None ->
    Core.eprintf "-anchor-scaled needs -anchor-norm DIR\n";
    Stdlib.exit 2
  | _ :: _, Some dir ->
    let from, to_ =
      match from, to_ with
      | Some from, Some to_ -> from, to_
      | Some d, None | None, Some d -> d, d
      | None, None ->
        Core.eprintf "-anchor-scaled needs -anchor-from DATE (and -anchor-to DATE)\n";
        Stdlib.exit 2
    in
    Deferred.List.map ~how:`Sequential scaled ~f:(fun s ->
      let reference, symbol, factor = parse_scaled s in
      let%map target = Anchoring.Target.load_norm ~factor ~dir reference ~from ~to_ in
      symbol, or_exit target)
;;

let load_anchors ~series ~fixed =
  let%map series =
    Deferred.List.map ~how:`Sequential series ~f:(fun s ->
      let file, symbol = assignment ~flag:"-anchor-series" s in
      let%map target = Anchoring.Target.load_series file in
      symbol, or_exit target)
  in
  let fixed =
    List.map fixed ~f:(fun s ->
      let spec, symbol = assignment ~flag:"-anchor-fixed" s in
      symbol, or_exit (Anchoring.Target.parse_fixed spec))
  in
  series @ fixed
;;

let command =
  Command.async
    ~summary:"Cboe US equities and options simulator (BOE v2 order entry, PITCH feed)"
    ~readme:(fun () ->
      "Order entry speaks BOE v2 on -gateway-port, the market data feed speaks PITCH 2.X \
       (Sequenced Unit Header frames) on -feed-port, and -http-port serves the same feed \
       as JSON on /ws. Any BOE username and password logs in. Unit 1 carries equities, \
       unit 2 SPY options. There is no real-time data for this venue: with -noise the \
       book is driven by noise traders. -anchor-series and -anchor-fixed pull a \
       listing's noise traders towards a reference price (mean reversion with \
       -anchor-half-life-ms) during core trading; auctions and LULD pauses own the price \
       while the anchor waits. Anchors turn -noise on.")
    (let%map_open.Command gateway_port =
       flag "-gateway-port" (optional_with_default 9300 int) ~doc:"PORT BOE port"
     and feed_port =
       flag "-feed-port" (optional_with_default 9301 int) ~doc:"PORT PITCH port"
     and http_port =
       flag "-http-port" (optional_with_default 9302 int) ~doc:"PORT HTTP/JSON websocket"
     and symbols =
       flag
         "-symbols"
         (optional (Arg_type.comma_separated string))
         ~doc:"SYM,... equities to list (default: AAPL,MSFT,NVDA,AMZN,SPY,ZVZZT)"
     and no_options = flag "-no-options" no_arg ~doc:" do not list the SPY options"
     and noise = flag "-noise" no_arg ~doc:" run noise traders so the books move"
     and noise_traders =
       flag
         "-noise-traders"
         (optional_with_default Venue.Config.default.noise_traders int)
         ~doc:"N noise traders per listing"
     and seed = flag "-seed" (optional_with_default 0 int) ~doc:"N noise trader seed"
     and clock =
       flag
         "-clock"
         (optional_with_default `Always_open clock_arg)
         ~doc:"MODE real, accelerated or always-open (default)"
     and clock_start =
       flag
         "-clock-start"
         (optional_with_default "09:25:00" string)
         ~doc:"HH:MM:SS New York time the accelerated clock starts at"
     and clock_date =
       flag
         "-clock-date"
         (optional date)
         ~doc:"DATE trading date of the accelerated clock (default: today or next)"
     and clock_speed =
       flag
         "-clock-speed"
         (optional_with_default 60. float)
         ~doc:"X accelerated clock speed (default 60)"
     and anchor_series =
       flag
         "-anchor-series"
         (listed string)
         ~doc:
           "FILE=SYM anchor SYM's noise traders to a series of '<time> <price>' lines \
            (repeatable)"
     and anchor_fixed =
       flag
         "-anchor-fixed"
         (listed string)
         ~doc:
           "PRICE[,DRIFT_BPS_PER_DAY]=SYM anchor SYM's noise traders to a fixed price \
            with an optional daily drift (repeatable)"
     and anchor_scaled =
       flag
         "-anchor-scaled"
         (listed string)
         ~doc:
           "VENUE:SYMBOL=SYM,FACTOR anchor SYM to a recorded reference (trade prices and \
            BBO mids from -anchor-norm) multiplied by FACTOR (repeatable)"
     and anchor_norm =
       flag
         "-anchor-norm"
         (optional string)
         ~doc:"DIR normalized event logs (the recorder's norm/ tree) for -anchor-scaled"
     and anchor_from =
       flag "-anchor-from" (optional date) ~doc:"DATE first UTC day for -anchor-scaled"
     and anchor_to =
       flag "-anchor-to" (optional date) ~doc:"DATE last UTC day (default: -anchor-from)"
     and anchor_half_life_ms =
       flag
         "-anchor-half-life-ms"
         (optional_with_default 1000 int)
         ~doc:"MS half-life of the pull towards the anchor (default 1000)"
     in
     fun () ->
       Log.Global.set_output [ Log.Output.stderr () ];
       let%bind anchors = load_anchors ~series:anchor_series ~fixed:anchor_fixed in
       let%bind scaled =
         load_scaled ~norm:anchor_norm ~from:anchor_from ~to_:anchor_to anchor_scaled
       in
       let anchors = anchors @ scaled in
       if anchor_half_life_ms <= 0
       then (
         Core.eprintf "-anchor-half-life-ms must be positive\n";
         Stdlib.exit 2);
       let instruments = Instruments.universe ?symbols ~options:(not no_options) () in
       List.iter anchors ~f:(fun (symbol, _) ->
         if not
              (List.exists instruments ~f:(fun (i : Instruments.t) ->
                 String.equal i.symbol symbol))
         then (
           Core.eprintf "anchor: %s is not listed\n" symbol;
           Stdlib.exit 2);
         if List.mem
              Equities_core.Symbol.Equity.default_test_symbols
              symbol
              ~equal:String.equal
         then (
           Core.eprintf "anchor: %s is a test symbol without noise traders\n" symbol;
           Stdlib.exit 2));
       let noise = noise || not (List.is_empty anchors) in
       List.iter anchors ~f:(fun (symbol, target) ->
         Log.Global.info_s
           [%message
             "anchor"
               (symbol : string)
               ~first_price:
                 (Anchoring.Target.price_at target ~elapsed:Time_ns.Span.zero
                  : Exchange_core.Price.t)]);
       let clock =
         match clock with
         | `Real -> Clock.real ()
         | `Always_open -> Clock.always_open ()
         | `Accelerated ->
           let date = Option.value_or_thunk clock_date ~default:trading_date in
           Clock.accelerated ~start:(Clock.ny_time date clock_start) ~speed:clock_speed
       in
       let%bind server =
         Server.start
           { gateway_port
           ; feed_port
           ; http_port
           ; instruments
           ; venue =
               { Venue.Config.default with
                 noise
               ; noise_traders
               ; seed
               ; anchors
               ; anchor_half_life = Time_ns.Span.of_int_ms anchor_half_life_ms
               }
           ; clock
           }
       in
       Log.Global.info
         "cboe-sim: BOE on %d, PITCH on %d, JSON websocket on http://localhost:%d/ws"
         (Server.gateway_port server)
         (Server.feed_port server)
         (Server.http_port server);
       Deferred.never ())
;;

let () = Command_unix.run command

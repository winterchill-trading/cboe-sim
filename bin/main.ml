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

let command =
  Command.async
    ~summary:"Cboe US equities and options simulator (BOE v2 order entry, PITCH feed)"
    ~readme:(fun () ->
      "Order entry speaks BOE v2 on -gateway-port, the market data feed speaks PITCH 2.X \
       (Sequenced Unit Header frames) on -feed-port, and -http-port serves the same feed \
       as JSON on /ws. Any BOE username and password logs in. Unit 1 carries equities, \
       unit 2 SPY options. There is no real-time data for this venue: with -noise the \
       book is driven by noise traders.")
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
     in
     fun () ->
       Log.Global.set_output [ Log.Output.stderr () ];
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
           ; instruments = Instruments.universe ?symbols ~options:(not no_options) ()
           ; venue = { Venue.Config.default with noise; noise_traders; seed }
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

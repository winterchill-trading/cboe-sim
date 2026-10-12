open! Core
open! Async
open Cboe_wire
open Cboe_sim

let connect ~host ~port =
  let%map _socket, reader, writer =
    Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port })
  in
  reader, writer
;;

(* BOE *)

let return_fields =
  Bitfields.bitfields_of_names
    Bitfields.return
    [ "Side"; "Price"; "Symbol"; "OrderQty"; "OrigClOrdID"; "LeavesQty" ]
;;

let boe_params =
  let%map_open.Command host =
    flag "-host" (optional_with_default "127.0.0.1" string) ~doc:"HOST simulator host"
  and port = flag "-port" (optional_with_default 9300 int) ~doc:"PORT BOE port"
  and user = flag "-user" (optional_with_default "TEST" string) ~doc:"USER 4 chars"
  and password =
    flag "-password" (optional_with_default "secret" string) ~doc:"PASSWORD any"
  and sub = flag "-sub" (optional_with_default "0001" string) ~doc:"ID session sub id"
  and wait =
    flag
      "-wait"
      (optional_with_default 1. float)
      ~doc:"SECONDS how long to print responses before logging out (default 1)"
  in
  host, port, user, password, sub, wait
;;

(* Logs in, prints every message, runs [act ~send] once logged in, waits [wait] seconds,
   then logs out. *)
let boe_session ?(replay = false) (host, port, user, password, sub, wait) ~act =
  let%bind reader, writer = connect ~host ~port in
  let next_seq = ref 1 in
  let logged_in = Ivar.create () in
  let logged_out = Ivar.create () in
  let send (m : Boe.Inbound.t) =
    let sequence =
      match m with
      | New_order _ | Cancel_order _ | Modify_order _ ->
        let s = !next_seq in
        incr next_seq;
        s
      | Login_request _ | Logout_request | Client_heartbeat -> 0
    in
    printf "> %s\n" (Pretty.boe_inbound m);
    Writer.write writer (Boe.Inbound.to_string ~sequence m)
  in
  send
    (Login_request
       { session_sub_id = sub
       ; username = user
       ; password
       ; unit_sequences =
           Some
             (if replay
              then { no_unspecified_unit_replay = false; units = [] }
              else { no_unspecified_unit_replay = true; units = [] })
       ; return_bitfields =
           List.map Boe.Message_type.with_return_bitfields ~f:(fun typ ->
             typ, return_fields)
       });
  let reading =
    Net.read_boe_frames reader ~f:(fun frame ->
      match Boe.Outbound.of_string frame with
      | Error e ->
        printf "! %s\n" (Error.to_string_hum e);
        true
      | Ok (_, _, Server_heartbeat) -> true
      | Ok (unit, seq, m) ->
        if seq > 0
        then printf "< [u%d #%d] %s\n" unit seq (Pretty.boe_outbound m)
        else printf "< %s\n" (Pretty.boe_outbound m);
        (match m with
         | Login_response { status = 'A'; last_received_sequence; _ } ->
           next_seq := last_received_sequence + 1
         | Login_response _ -> Ivar.fill_if_empty logged_out ()
         | Replay_complete -> Ivar.fill_if_empty logged_in ()
         | Logout _ -> Ivar.fill_if_empty logged_out ()
         | _ -> ());
        not (Ivar.is_full logged_out))
  in
  don't_wait_for (reading >>| fun () -> Ivar.fill_if_empty logged_out ());
  let heartbeats = Ivar.create () in
  Clock_ns.every ~stop:(Ivar.read heartbeats) Time_ns.Span.second (fun () ->
    if not (Writer.is_closed writer)
    then Writer.write writer (Boe.Inbound.to_string ~sequence:0 Client_heartbeat));
  let%bind () =
    match%bind
      Clock_ns.with_timeout
        (Time_ns.Span.of_sec 5.)
        (Deferred.any [ Ivar.read logged_in; Ivar.read logged_out ])
    with
    | `Timeout -> return (printf "! no login response\n")
    | `Result () ->
      if Ivar.is_full logged_in && not (Ivar.is_full logged_out)
      then (
        act ~send;
        let%bind () = Clock_ns.after (Time_ns.Span.of_sec wait) in
        send Logout_request;
        Clock_ns.with_timeout (Time_ns.Span.of_sec 2.) (Ivar.read logged_out)
        >>| (ignore : unit Clock_ns.Or_timeout.t -> unit))
      else return ()
  in
  Ivar.fill_if_empty heartbeats ();
  Writer.close writer
;;

let logon =
  Command.async
    ~summary:"Log in to the BOE port, print the response (and replay), log out"
    (let%map_open.Command params = boe_params
     and replay =
       flag "-replay" no_arg ~doc:" ask for every sequenced message of this session"
     in
     fun () -> boe_session ~replay params ~act:(fun ~send:_ -> ()))
;;

let side_arg =
  Command.Arg_type.of_alist_exn
    [ "buy", '1'; "sell", '2'; "short", '5'; "short-exempt", '6' ]
;;

let tif_arg =
  Command.Arg_type.of_alist_exn
    [ "day", '0'; "gtc", '1'; "ioc", '3'; "fok", '4'; "opg", '2'; "clo", '7' ]
;;

let default_id () =
  sprintf "C%d" (Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1000 % 1_000_000_000)
;;

let order =
  Command.async
    ~summary:"Send a New Order and print the responses"
    (let%map_open.Command params = boe_params
     and symbol = flag "-symbol" (required string) ~doc:"SYM ticker or option feed symbol"
     and side =
       flag "-side" (required side_arg) ~doc:"SIDE buy, sell, short, short-exempt"
     and qty = flag "-qty" (required int) ~doc:"N shares or contracts"
     and price = flag "-price" (optional string) ~doc:"PX limit price (omit for market)"
     and tif = flag "-tif" (optional tif_arg) ~doc:"TIF day, gtc, ioc, fok, opg, clo"
     and id = flag "-id" (optional string) ~doc:"CLORDID client order id"
     and max_floor =
       flag "-max-floor" (optional int) ~doc:"N displayed quantity of a reserve order"
     and hidden = flag "-hidden" no_arg ~doc:" do not display the order"
     and post_only = flag "-post-only" no_arg ~doc:" RoutingInst P (add liquidity only)"
     and stp = flag "-stp" (optional string) ~doc:"MODE PreventMatch, e.g. NF, OF, BF" in
     fun () ->
       let cl_ord_id = Option.value_or_thunk id ~default:default_id in
       let n : Boe.New_order.t =
         { (Boe.New_order.create ~cl_ord_id ~side ~order_qty:qty ~symbol ()) with
           price = Option.map price ~f:Price4.of_string
         ; ord_type = Some (if Option.is_some price then '2' else '1')
         ; time_in_force = tif
         ; max_floor
         ; display_indicator = (if hidden then Some 'I' else None)
         ; routing_inst = (if post_only then Some "P" else None)
         ; prevent_match = stp
         }
       in
       boe_session params ~act:(fun ~send -> send (New_order n)))
;;

let cancel =
  Command.async
    ~summary:"Cancel an order of this session by ClOrdID"
    (let%map_open.Command params = boe_params
     and id = flag "-id" (required string) ~doc:"CLORDID order to cancel" in
     fun () ->
       boe_session params ~act:(fun ~send -> send (Cancel_order { orig_cl_ord_id = id })))
;;

let modify =
  Command.async
    ~summary:"Modify the price or size of an order of this session"
    (let%map_open.Command params = boe_params
     and id = flag "-id" (required string) ~doc:"CLORDID order to modify"
     and new_id = flag "-new-id" (required string) ~doc:"CLORDID id after the modify"
     and qty = flag "-qty" (optional int) ~doc:"N new order quantity"
     and price = flag "-price" (optional string) ~doc:"PX new price" in
     fun () ->
       boe_session params ~act:(fun ~send ->
         send
           (Modify_order
              { cl_ord_id = new_id
              ; orig_cl_ord_id = id
              ; order_qty = qty
              ; price = Option.map price ~f:Price4.of_string
              ; ord_type = None
              ; cancel_orig_on_reject = None
              ; side = None
              ; max_floor = None
              })))
;;

(* PITCH *)

let feed_params =
  let%map_open.Command host =
    flag "-host" (optional_with_default "127.0.0.1" string) ~doc:"HOST simulator host"
  and port = flag "-port" (optional_with_default 9301 int) ~doc:"PORT PITCH port"
  and seconds =
    flag "-seconds" (optional_with_default 2. float) ~doc:"S how long to listen"
  in
  host, port, seconds
;;

let read_feed (host, port, seconds) ~f =
  let%bind reader, writer = connect ~host ~port in
  let stop = Ivar.create () in
  don't_wait_for
    (Net.read_pitch_frames reader ~f:(fun s ->
       (match Pitch.Frame.of_string s with
        | Ok frame -> f frame
        | Error e -> printf "! %s\n" (Error.to_string_hum e));
       not (Ivar.is_full stop)));
  let%bind () = Clock_ns.after (Time_ns.Span.of_sec seconds) in
  Ivar.fill_if_empty stop ();
  Writer.close writer
;;

let feed =
  Command.async
    ~summary:"Print decoded PITCH frames: definitions, spin, then live messages"
    (let%map_open.Command params = feed_params
     and heartbeats = flag "-heartbeats" no_arg ~doc:" also print heartbeats"
     and no_time = flag "-no-time" no_arg ~doc:" hide Time messages" in
     fun () ->
       read_feed params ~f:(fun (frame : Pitch.Frame.t) ->
         if List.is_empty frame.messages && heartbeats
         then printf "[u%d #%d] heartbeat\n" frame.unit frame.sequence;
         List.iteri frame.messages ~f:(fun i m ->
           match m with
           | Time _ when no_time -> ()
           | _ ->
             if frame.sequence = 0
             then printf "[u%d spin] %s\n" frame.unit (Pretty.pitch m)
             else printf "[u%d #%d] %s\n" frame.unit (frame.sequence + i) (Pretty.pitch m))))
;;

let book =
  Command.async
    ~summary:"Build the book of a symbol from the feed and print it"
    (let%map_open.Command params = feed_params
     and symbol = flag "-symbol" (required string) ~doc:"SYM ticker or option feed symbol"
     and depth = flag "-depth" (optional_with_default 5 int) ~doc:"N levels per side" in
     fun () ->
       let b = Pitch_book.create () in
       let%map () =
         read_feed params ~f:(fun frame ->
           List.iter frame.messages ~f:(Pitch_book.apply b ~unit:frame.unit))
       in
       let side bids = List.take (Pitch_book.levels b ~symbol ~bids) depth in
       printf "%s book\n" symbol;
       let asks = List.rev (side false) in
       List.iter asks ~f:(fun (px, q, n) ->
         printf "  ask %10s %8d (%d)\n" (Price4.to_string px) q n);
       printf "  ----\n";
       List.iter (side true) ~f:(fun (px, q, n) ->
         printf "  bid %10s %8d (%d)\n" (Price4.to_string px) q n))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Play with cboe-sim: BOE order entry and PITCH feed"
       [ "logon", logon
       ; "order", order
       ; "cancel", cancel
       ; "modify", modify
       ; "feed", feed
       ; "book", book
       ])
;;

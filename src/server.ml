open! Core
module Sim_clock = Clock
open! Async

module Config = struct
  type t =
    { gateway_port : int
    ; feed_port : int
    ; http_port : int
    ; instruments : Instruments.t list
    ; venue : Venue.Config.t
    ; clock : Sim_clock.t
    }
end

type t =
  { venue : Venue.t
  ; feed : Feed.t
  ; gateway : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; feed_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; http : (Socket.Address.Inet.t, int) Cohttp_async.Server.t
  ; stop : unit Ivar.t
  }

let gateway_port t = Tcp.Server.listening_on t.gateway
let feed_port t = Tcp.Server.listening_on t.feed_server
let http_port t = Cohttp_async.Server.listening_on t.http
let venue t = t.venue
let feed t = t.feed
let write writer s = if not (Writer.is_closed writer) then Writer.write writer s

let handle_gateway ~store ~venue _addr reader writer =
  let closed = Ivar.create () in
  let handler =
    Boe_session.Handler.create
      ~store
      ~venue
      ~now:Time_ns.now
      ~send:(write writer)
      ~close:(fun () -> Ivar.fill_if_empty closed ())
      ()
  in
  Clock_ns.every ~stop:(Ivar.read closed) (Time_ns.Span.of_int_ms 100) (fun () ->
    Boe_session.Handler.on_timer handler);
  let%bind () =
    Deferred.any
      [ Net.read_boe_frames reader ~f:(fun frame ->
          Boe_session.Handler.on_frame handler frame;
          not (Boe_session.Handler.is_closed handler))
      ; Ivar.read closed
      ]
  in
  Boe_session.Handler.on_disconnect handler;
  Ivar.fill_if_empty closed ();
  Writer.flushed writer
;;

let handle_feed ~feed ~venue _addr reader writer =
  let sub =
    Feed.subscribe feed ~trading_status:(Venue.trading_status venue) ~send:(write writer)
  in
  (* Clients may send heartbeats; their content is ignored. *)
  let%bind () = Deferred.any [ Reader.drain reader; Writer.consumer_left writer ] in
  Feed.unsubscribe feed sub;
  return ()
;;

let header_is req name value =
  match Cohttp.Header.get (Cohttp.Request.headers req) name with
  | Some v ->
    String.split v ~on:','
    |> List.exists ~f:(fun v -> String.Caseless.equal (String.strip v) value)
  | None -> false
;;

let handle_ws ~feed ~venue reader writer =
  let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
  let incoming, outgoing = Websocket.pipes ws in
  let sub =
    Feed.subscribe_json feed ~trading_status:(Venue.trading_status venue) ~send:(fun s ->
      Pipe.write_without_pushback_if_open outgoing s)
  in
  let%map () = Pipe.iter_without_pushback incoming ~f:ignore in
  Feed.unsubscribe feed sub;
  Pipe.close outgoing
;;

let http_handler ~feed ~venue ~body:_ _addr req =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path, Cohttp.Header.get (Cohttp.Request.headers req) "sec-websocket-key" with
  | ("/ws" | "/ws/"), Some key when header_is req "upgrade" "websocket" ->
    let headers =
      Cohttp.Header.of_list
        [ "upgrade", "websocket"
        ; "connection", "Upgrade"
        ; ( "sec-websocket-accept"
          , Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key )
        ]
    in
    let response = Http.Response.make ~status:`Switching_protocols ~headers () in
    return (`Expert (response, fun reader writer -> handle_ws ~feed ~venue reader writer))
  | _ ->
    let clock = Venue.clock venue in
    let body =
      `Assoc
        [ "venue", `String "cboe-sim"
        ; ( "time"
          , `String (Equities_core.Session.to_local_string New_york (Sim_clock.now clock))
          )
        ; ( "phase"
          , `String
              (Sexp.to_string
                 (Equities_core.Session.Us_equities.Phase.sexp_of_t
                    (Sim_clock.phase clock))) )
        ; ( "symbols"
          , `List
              (List.map (Venue.instruments venue) ~f:(fun (i : Instruments.t) ->
                 `String i.symbol)) )
        ; "websocket", `String "/ws"
        ]
    in
    let%map response =
      Cohttp_async.Server.respond_string
        ~headers:(Cohttp.Header.of_list [ "content-type", "application/json" ])
        (Yojson.Safe.to_string body)
    in
    `Response response
;;

let start (config : Config.t) =
  let store = Boe_session.Store.create () in
  let feed = Feed.create ~clock:config.clock config.instruments in
  let venue =
    Venue.create
      ~config:config.venue
      ~clock:config.clock
      ~emit_user:(Boe_session.Store.deliver store)
      ~emit_feed:(Feed.publish feed)
      config.instruments
  in
  let%bind gateway =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port config.gateway_port)
      (handle_gateway ~store ~venue)
  in
  let%bind feed_server =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port config.feed_port)
      (handle_feed ~feed ~venue)
  in
  let%map http =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call (fun _ exn -> Log.Global.error_s [%message "http handler" (exn : exn)]))
      (Tcp.Where_to_listen.of_port config.http_port)
      (http_handler ~feed ~venue)
  in
  let stop = Ivar.create () in
  Clock_ns.every ~stop:(Ivar.read stop) (Time_ns.Span.of_int_ms 100) (fun () ->
    Venue.tick venue;
    Feed.on_timer feed);
  { venue; feed; gateway; feed_server; http; stop }
;;

let close t =
  Ivar.fill_if_empty t.stop ();
  let%bind () = Tcp.Server.close t.gateway in
  let%bind () = Tcp.Server.close t.feed_server in
  Cohttp_async.Server.close t.http
;;

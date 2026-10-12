open! Core
open Cboe_wire

let units = [ Instruments.equities_unit; Instruments.options_unit ]

module Store = struct
  type live = { send : unit:int -> sequence:int -> Boe.Outbound.t -> unit }

  type entry =
    { mutable last_inbound : int
    ; seqs : int Int.Table.t
    ; log : (int * int * Boe.Outbound.t) Queue.t
    ; mutable live : live option
    }

  type t = entry String.Table.t

  let create () = String.Table.create ()
  let key ~username ~session_sub_id = username ^ "/" ^ session_sub_id

  let entry t user =
    Hashtbl.find_or_add t user ~default:(fun () ->
      { last_inbound = 0; seqs = Int.Table.create (); log = Queue.create (); live = None })
  ;;

  let deliver t ~user ~unit msg =
    let e = entry t user in
    if Boe.Outbound.is_sequenced msg
    then (
      let sequence = 1 + Option.value (Hashtbl.find e.seqs unit) ~default:0 in
      Hashtbl.set e.seqs ~key:unit ~data:sequence;
      Queue.enqueue e.log (unit, sequence, msg);
      Option.iter e.live ~f:(fun l -> l.send ~unit ~sequence msg))
    else Option.iter e.live ~f:(fun l -> l.send ~unit:0 ~sequence:0 msg)
  ;;

  let unit_sequences t ~user =
    let e = entry t user in
    List.map units ~f:(fun u -> u, Option.value (Hashtbl.find e.seqs u) ~default:0)
  ;;
end

module Handler = struct
  type state =
    | Awaiting_login
    | Logged_in of
        { user : string
        ; return_bitfields : (int * string) list
        }
    | Closed

  type t =
    { store : Store.t
    ; venue : Venue.t
    ; now : unit -> Time_ns.t
    ; send_raw : string -> unit
    ; close_raw : unit -> unit
    ; heartbeat_timeout : Time_ns.Span.t
    ; mutable state : state
    ; mutable last_sent : Time_ns.t
    ; mutable last_received : Time_ns.t
    }

  let create
    ?(heartbeat_timeout = Time_ns.Span.of_sec 5.)
    ~store
    ~venue
    ~now
    ~send
    ~close
    ()
    =
    { store
    ; venue
    ; now
    ; send_raw = send
    ; close_raw = close
    ; heartbeat_timeout
    ; state = Awaiting_login
    ; last_sent = now ()
    ; last_received = now ()
    }
  ;;

  let is_closed t =
    match t.state with
    | Closed -> true
    | Awaiting_login | Logged_in _ -> false
  ;;

  let write t ~return_bitfields ~unit ~sequence msg =
    t.last_sent <- t.now ();
    t.send_raw
      (Boe.Outbound.to_string
         ~matching_unit:unit
         ~sequence
         ~return_bitfields:(fun typ ->
           List.Assoc.find return_bitfields typ ~equal:Int.equal
           |> Option.value ~default:"")
         msg)
  ;;

  let session_msg t msg = write t ~return_bitfields:[] ~unit:0 ~sequence:0 msg

  let detach t =
    match t.state with
    | Logged_in { user; _ } -> (Store.entry t.store user).live <- None
    | Awaiting_login | Closed -> ()
  ;;

  let close t =
    if not (is_closed t)
    then (
      detach t;
      t.state <- Closed;
      t.close_raw ())
  ;;

  let logout t ~reason ~text =
    let last_received_sequence, units =
      match t.state with
      | Logged_in { user; _ } ->
        (Store.entry t.store user).last_inbound, Store.unit_sequences t.store ~user
      | Awaiting_login | Closed -> 0, []
    in
    session_msg t (Logout { reason; text; last_received_sequence; units });
    close t
  ;;

  let login_status t (r : Boe.Login_request.t) ~user =
    if String.is_empty r.username
    then Error ('N', "username required")
    else if Option.is_some (Store.entry t.store user).live
    then Error ('B', "session in use")
    else (
      match
        List.find r.return_bitfields ~f:(fun (typ, bf) ->
          (not (List.mem Boe.Message_type.with_return_bitfields typ ~equal:Int.equal))
          || Or_error.is_error (Bitfields.names_of_bitfields Bitfields.return bf))
      with
      | Some (typ, _) -> Error ('F', sprintf "invalid return bitfields for 0x%02X" typ)
      | None -> Ok ())
  ;;

  let login t (r : Boe.Login_request.t) =
    let user = Store.key ~username:r.username ~session_sub_id:r.session_sub_id in
    let e = Store.entry t.store user in
    match login_status t r ~user with
    | Error (status, text) ->
      session_msg
        t
        (Login_response
           { status
           ; text
           ; no_unspecified_unit_replay = false
           ; last_received_sequence = 0
           ; units = []
           ; return_bitfields = []
           });
      close t
    | Ok () ->
      let return_bitfields = r.return_bitfields in
      session_msg
        t
        (Login_response
           { status = 'A'
           ; text = "accepted"
           ; no_unspecified_unit_replay =
               Option.value_map r.unit_sequences ~default:false ~f:(fun u ->
                 u.no_unspecified_unit_replay)
           ; last_received_sequence = e.last_inbound
           ; units = Store.unit_sequences t.store ~user
           ; return_bitfields
           });
      let replay_from unit =
        Option.bind r.unit_sequences ~f:(fun (us : Boe.Unit_sequences.t) ->
          match List.Assoc.find us.units unit ~equal:Int.equal with
          | Some s -> Some s
          | None -> if us.no_unspecified_unit_replay then None else Some 0)
      in
      let from = List.map units ~f:(fun u -> u, replay_from u) in
      Queue.iter e.log ~f:(fun (unit, sequence, msg) ->
        match List.Assoc.find from unit ~equal:Int.equal with
        | Some (Some last) when sequence > last ->
          write t ~return_bitfields ~unit ~sequence msg
        | _ -> ());
      session_msg t Replay_complete;
      t.state <- Logged_in { user; return_bitfields };
      e.live
      <- Some
           { send =
               (fun ~unit ~sequence msg ->
                 if not (is_closed t) then write t ~return_bitfields ~unit ~sequence msg)
           }
  ;;

  let application t ~user ~sequence (msg : Boe.Inbound.t) =
    let e = Store.entry t.store user in
    (* A sequence already processed is a resend after reconnect: ignore it. *)
    if sequence = 0 || sequence > e.last_inbound
    then (
      if sequence > 0 then e.last_inbound <- sequence;
      match msg with
      | New_order n -> Venue.new_order t.venue ~user n
      | Cancel_order c -> Venue.cancel_order t.venue ~user c
      | Modify_order m -> Venue.modify_order t.venue ~user m
      | Login_request _ | Logout_request | Client_heartbeat -> ())
  ;;

  let on_frame t frame =
    t.last_received <- t.now ();
    match t.state with
    | Closed -> ()
    | Awaiting_login ->
      (match Boe.Inbound.of_string frame with
       | Ok (_, Login_request r) -> login t r
       | Ok (_, _) -> logout t ~reason:'!' ~text:"login required"
       | Error e -> logout t ~reason:'!' ~text:(Error.to_string_hum e))
    | Logged_in { user; _ } ->
      (match Boe.Inbound.of_string frame with
       | Error e -> logout t ~reason:'!' ~text:(Error.to_string_hum e)
       | Ok (_, Client_heartbeat) -> ()
       | Ok (_, Logout_request) -> logout t ~reason:'U' ~text:"user requested"
       | Ok (_, Login_request _) -> logout t ~reason:'!' ~text:"already logged in"
       | Ok (sequence, ((New_order _ | Cancel_order _ | Modify_order _) as msg)) ->
         application t ~user ~sequence msg)
  ;;

  let on_timer t =
    if not (is_closed t)
    then (
      let now = t.now () in
      if Time_ns.Span.(Time_ns.diff now t.last_received > t.heartbeat_timeout)
      then (
        match t.state with
        | Logged_in _ -> logout t ~reason:'!' ~text:"heartbeat timeout"
        | Awaiting_login | Closed -> close t)
      else if Time_ns.Span.(Time_ns.diff now t.last_sent >= second)
      then (
        match t.state with
        | Logged_in _ -> session_msg t Server_heartbeat
        | Awaiting_login | Closed -> ()))
  ;;

  let on_disconnect t =
    detach t;
    t.state <- Closed
  ;;
end

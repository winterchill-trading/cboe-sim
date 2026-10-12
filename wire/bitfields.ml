open! Core

module Spec = struct
  type t =
    { name : string
    ; byte : int
    ; bit : int
    ; len : int
    }
  [@@deriving sexp_of]
end

type table = Spec.t list

let spec name byte bit len = { Spec.name; byte; bit; len }

let new_order =
  [ spec "ClearingFirm" 1 1 4
  ; spec "ClearingAccount" 1 2 4
  ; spec "Price" 1 4 8
  ; spec "ExecInst" 1 8 1
  ; spec "OrdType" 1 16 1
  ; spec "TimeInForce" 1 32 1
  ; spec "MinQty" 1 64 4
  ; spec "MaxFloor" 1 128 4
  ; spec "Symbol" 2 1 8
  ; spec "SymbolSfx" 2 2 8
  ; spec "Currency" 2 4 3
  ; spec "IdSource" 2 8 1
  ; spec "SecurityId" 2 16 16
  ; spec "SecurityExchange" 2 32 4
  ; spec "Capacity" 2 64 1
  ; spec "RoutingInst" 2 128 4
  ; spec "Account" 3 1 16
  ; spec "DisplayIndicator" 3 2 1
  ; spec "DiscretionAmount" 3 8 2
  ; spec "PegDifference" 3 16 8
  ; spec "PreventMatch" 3 32 3
  ; spec "LocateReqd" 3 64 1
  ; spec "ExpireTime" 3 128 8
  ; spec "MaturityDate" 4 1 4
  ; spec "StrikePrice" 4 2 8
  ; spec "PutOrCall" 4 4 1
  ; spec "RiskReset" 4 8 8
  ; spec "OpenClose" 4 16 1
  ; spec "AttributedQuote" 5 2 1
  ; spec "ExtExecInst" 5 8 1
  ; spec "DisplayRange" 6 1 4
  ; spec "StopPx" 6 2 8
  ; spec "EchoText" 6 32 64
  ]
;;

let modify_order =
  [ spec "ClearingFirm" 1 1 4
  ; spec "OrderQty" 1 4 4
  ; spec "Price" 1 8 8
  ; spec "OrdType" 1 16 1
  ; spec "CancelOrigOnReject" 1 32 1
  ; spec "ExecInst" 1 64 1
  ; spec "Side" 1 128 1
  ; spec "MaxFloor" 2 1 4
  ; spec "StopPx" 2 2 8
  ]
;;

let cancel_order = [ spec "ClearingFirm" 1 1 4 ]

let return =
  [ spec "Side" 1 1 1
  ; spec "PegDifference" 1 2 8
  ; spec "Price" 1 4 8
  ; spec "ExecInst" 1 8 1
  ; spec "OrdType" 1 16 1
  ; spec "TimeInForce" 1 32 1
  ; spec "MinQty" 1 64 4
  ; spec "MaxRemovePct" 1 128 1
  ; spec "Symbol" 2 1 8
  ; spec "SymbolSfx" 2 2 8
  ; spec "Currency" 2 4 3
  ; spec "IdSource" 2 8 1
  ; spec "SecurityId" 2 16 16
  ; spec "SecurityExchange" 2 32 4
  ; spec "Capacity" 2 64 1
  ; spec "Account" 3 1 16
  ; spec "ClearingFirm" 3 2 4
  ; spec "ClearingAccount" 3 4 4
  ; spec "DisplayIndicator" 3 8 1
  ; spec "MaxFloor" 3 16 4
  ; spec "DiscretionAmount" 3 32 2
  ; spec "OrderQty" 3 64 4
  ; spec "PreventMatch" 3 128 3
  ; spec "MaturityDate" 4 1 4
  ; spec "StrikePrice" 4 2 8
  ; spec "PutOrCall" 4 4 1
  ; spec "OpenClose" 4 8 1
  ; spec "OrigClOrdID" 5 1 20
  ; spec "LeavesQty" 5 2 4
  ; spec "LastShares" 5 4 4
  ; spec "LastPx" 5 8 8
  ; spec "DisplayPrice" 5 16 8
  ; spec "WorkingPrice" 5 32 8
  ; spec "BaseLiquidityIndicator" 5 64 1
  ; spec "ExpireTime" 5 128 8
  ]
;;

module Raw = struct
  let le n v = String.init n ~f:(fun i -> Char.of_int_exn ((v lsr (8 * i)) land 0xff))
  let u8 = le 1
  let u16 = le 2
  let u32 = le 4
  let i64 = le 8
  let char c = String.of_char c

  let text ~len s =
    String.init len ~f:(fun i -> if i < String.length s then s.[i] else '\000')
  ;;

  let to_int s =
    let b = Bigstring.of_string s in
    match String.length s with
    | 1 -> Le.get_u8 b ~pos:0
    | 2 -> Le.get_u16 b ~pos:0
    | 4 -> Le.get_u32 b ~pos:0
    | 8 -> Le.get_i64 b ~pos:0
    | n -> raise_s [%message "Bitfields.Raw.to_int: unsupported width" (n : int)]
  ;;

  let to_char s = if String.is_empty s then '\000' else s.[0]

  let to_text s =
    String.prefix s (Option.value (String.index s '\000') ~default:(String.length s))
  ;;
end

let find table name =
  match List.find table ~f:(fun (s : Spec.t) -> String.equal s.name name) with
  | Some s -> s
  | None -> raise_s [%message "Bitfields: unknown field" (name : string)]
;;

let bitfields_of_names table names =
  let specs = List.map names ~f:(find table) in
  let n = List.fold specs ~init:0 ~f:(fun acc s -> Int.max acc s.byte) in
  let b = Bytes.make n '\000' in
  List.iter specs ~f:(fun s ->
    let i = s.byte - 1 in
    Bytes.set b i (Char.of_int_exn (Char.to_int (Bytes.get b i) lor s.bit)));
  Bytes.to_string b
;;

let names_of_bitfields table bitfields =
  let set =
    List.concat_mapi (String.to_list bitfields) ~f:(fun i c ->
      List.filter_map [ 1; 2; 4; 8; 16; 32; 64; 128 ] ~f:(fun bit ->
        if Char.to_int c land bit <> 0 then Some (i + 1, bit) else None))
  in
  List.map set ~f:(fun (byte, bit) ->
    match List.find table ~f:(fun (s : Spec.t) -> s.byte = byte && s.bit = bit) with
    | Some s -> Ok s.name
    | None -> Or_error.error_s [%message "unsupported bitfield" (byte : int) (bit : int)])
  |> Or_error.all
;;

let specs_of_bitfields table bitfields =
  names_of_bitfields table bitfields |> Or_error.map ~f:(List.map ~f:(find table))
;;

let size table ~bitfields =
  let specs = Or_error.ok_exn (specs_of_bitfields table bitfields) in
  1 + String.length bitfields + List.sum (module Int) specs ~f:(fun s -> s.len)
;;

let write table buf ~pos ~bitfields ~lookup =
  let specs = Or_error.ok_exn (specs_of_bitfields table bitfields) in
  Le.set_u8 buf ~pos (String.length bitfields);
  String.iteri bitfields ~f:(fun i c -> Le.set_char buf ~pos:(pos + 1 + i) c);
  let p = ref (pos + 1 + String.length bitfields) in
  List.iter specs ~f:(fun (s : Spec.t) ->
    let v = Option.value (lookup s.name) ~default:"" in
    for i = 0 to s.len - 1 do
      Le.set_char buf ~pos:(!p + i) (if i < String.length v then v.[i] else '\000')
    done;
    p := !p + s.len);
  !p - pos
;;

let read table buf ~pos ~len =
  let open Or_error.Let_syntax in
  let limit = pos + len in
  let%bind () =
    if pos < limit then Ok () else Or_error.error_string "missing bitfield count"
  in
  let n = Le.get_u8 buf ~pos in
  let%bind () =
    if pos + 1 + n <= limit then Ok () else Or_error.error_string "truncated bitfields"
  in
  let bitfields = Bigstring.To_string.sub buf ~pos:(pos + 1) ~len:n in
  let%bind specs = specs_of_bitfields table bitfields in
  let p = ref (pos + 1 + n) in
  let%map fields =
    List.map specs ~f:(fun (s : Spec.t) ->
      if !p + s.len > limit
      then Or_error.error_s [%message "truncated optional field" s.name]
      else (
        let v = Bigstring.To_string.sub buf ~pos:!p ~len:s.len in
        p := !p + s.len;
        Ok (s.name, v)))
    |> Or_error.all
  in
  bitfields, fields
;;

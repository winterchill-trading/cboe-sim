open! Core
open Cboe_wire

let%expect_test "bitfield bytes follow the specification tables" =
  let show names =
    let b = Bitfields.bitfields_of_names Bitfields.new_order names in
    printf "%-40s %s\n" (String.concat ~sep:"," names) (Le.hex b)
  in
  show [ "Price"; "OrdType"; "TimeInForce" ];
  show [ "Symbol"; "Capacity" ];
  show [ "Price"; "Symbol"; "Capacity"; "PreventMatch" ];
  show [ "MaturityDate"; "StrikePrice"; "PutOrCall" ];
  [%expect
    {|
    Price,OrdType,TimeInForce                34
    Symbol,Capacity                          00 41
    Price,Symbol,Capacity,PreventMatch       04 41 20
    MaturityDate,StrikePrice,PutOrCall       00 00 00 07
    |}]
;;

let%expect_test "fields are written in bit order whatever the lookup order" =
  let table = Bitfields.new_order in
  let bitfields = Bitfields.bitfields_of_names table [ "Symbol"; "Price"; "Capacity" ] in
  let buf = Bigstring.create (Bitfields.size table ~bitfields) in
  let values =
    [ "Capacity", Bitfields.Raw.char 'A'
    ; "Symbol", Bitfields.Raw.text ~len:8 "AAPL"
    ; "Price", Bitfields.Raw.i64 1_895_000
    ]
  in
  let n =
    Bitfields.write table buf ~pos:0 ~bitfields ~lookup:(fun name ->
      List.Assoc.find values name ~equal:String.equal)
  in
  print_endline (Le.hex (Bigstring.to_string buf));
  let bf, fields = Bitfields.read table buf ~pos:0 ~len:n |> Or_error.ok_exn in
  print_s [%sexp (Le.hex bf : string), (List.map fields ~f:fst : string list)];
  print_s
    [%sexp
      (Bitfields.Raw.to_int (List.Assoc.find_exn fields ~equal:String.equal "Price")
       : int)];
  [%expect
    {|
    02 04 41 58 ea 1c 00 00 00 00 00 41 41 50 4c 00 00 00 00 41
    ("04 41" (Price Symbol Capacity))
    1895000
    |}]
;;

let%expect_test "unknown bits are rejected" =
  print_s
    [%sexp
      (Bitfields.names_of_bitfields Bitfields.return "\000\128" : string list Or_error.t)];
  [%expect {| (Error ("unsupported bitfield" (byte 2) (bit 128))) |}]
;;

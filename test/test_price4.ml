open! Core
open Cboe_wire

let%expect_test "4 implied decimals" =
  List.iter [ "123.45"; "0.0001"; "100"; "-1.5"; "189.5" ] ~f:(fun s ->
    let p = Price4.of_string s in
    printf "%s -> %d -> %s\n" s p (Price4.to_string p));
  [%expect
    {|
    123.45 -> 1234500 -> 123.45
    0.0001 -> 1 -> 0.0001
    100 -> 1000000 -> 100
    -1.5 -> -15000 -> -1.5
    189.5 -> 1895000 -> 189.5
    |}];
  print_s [%sexp (Price4.to_short_exn 1234500 : int)];
  [%expect {| 12345 |}]
;;

let%expect_test "padding conventions" =
  let b = Bigstring.create 8 in
  Le.set_text b ~pad:Nul ~pos:0 ~len:4 "AB";
  Le.set_text b ~pad:Space ~pos:4 ~len:4 "CD";
  print_endline (Le.hex (Bigstring.to_string b));
  printf
    "%S %S\n"
    (Le.get_text b ~pad:Nul ~pos:0 ~len:4)
    (Le.get_text b ~pad:Space ~pos:4 ~len:4);
  [%expect {|
    41 42 00 00 43 44 20 20
    "AB" "CD"
    |}]
;;

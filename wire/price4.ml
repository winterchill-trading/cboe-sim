open! Core

type t = int [@@deriving sexp, compare, equal]

let of_string s =
  let neg, s =
    match String.chop_prefix s ~prefix:"-" with
    | Some s -> true, s
    | None -> false, s
  in
  let whole, frac =
    match String.lsplit2 s ~on:'.' with
    | Some (w, f) -> w, f
    | None -> s, ""
  in
  if String.length frac > 4
  then raise_s [%message "Price4.of_string: more than 4 decimals" (s : string)];
  let frac = frac ^ String.make (4 - String.length frac) '0' in
  let v =
    (Int.of_string (if String.is_empty whole then "0" else whole) * 10_000)
    + Int.of_string frac
  in
  if neg then -v else v
;;

let to_string t =
  let sign = if t < 0 then "-" else "" in
  let a = Int.abs t in
  let whole = a / 10_000 in
  let frac = a % 10_000 in
  if frac = 0
  then sprintf "%s%d" sign whole
  else
    sprintf
      "%s%d.%s"
      sign
      whole
      (String.rstrip (sprintf "%04d" frac) ~drop:(Char.equal '0'))
;;

let to_short_exn t =
  if t % 100 <> 0 then raise_s [%message "Price4.to_short_exn: sub-cent price" (t : int)];
  t / 100
;;

let of_short s = s * 100

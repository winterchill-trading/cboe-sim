open! Core

let get_u8 b ~pos = Bigstring.get_uint8 b ~pos
let set_u8 b ~pos v = Bigstring.set_uint8_exn b ~pos v
let get_u16 b ~pos = Bigstring.get_uint16_le b ~pos
let set_u16 b ~pos v = Bigstring.set_uint16_le_exn b ~pos v
let get_u32 b ~pos = Bigstring.get_uint32_le b ~pos
let set_u32 b ~pos v = Bigstring.set_uint32_le_exn b ~pos v
let get_i64 b ~pos = Bigstring.get_int64_le_exn b ~pos
let set_i64 b ~pos v = Bigstring.set_int64_le b ~pos v
let get_u64 b ~pos = Bigstring.get_uint64_le_exn b ~pos
let set_u64 b ~pos v = Bigstring.set_uint64_le_exn b ~pos v
let get_char b ~pos = Bigstring.get b pos
let set_char b ~pos c = Bigstring.set b pos c

module Pad = struct
  type t =
    | Nul
    | Space

  let char = function
    | Nul -> '\000'
    | Space -> ' '
  ;;
end

let set_text b ~pad ~pos ~len s =
  for i = 0 to len - 1 do
    Bigstring.set b (pos + i) (if i < String.length s then s.[i] else Pad.char pad)
  done
;;

let get_text b ~pad ~pos ~len =
  let s = Bigstring.To_string.sub b ~pos ~len in
  (* A NUL ends BOE text; PITCH strips trailing spaces. *)
  match (pad : Pad.t) with
  | Nul -> String.prefix s (Option.value (String.index s '\000') ~default:len)
  | Space -> String.rstrip s ~drop:(Char.equal ' ')
;;

let hex s =
  String.to_list s
  |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep:" "
;;

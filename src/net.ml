open! Core
open! Async
open Cboe_wire

let read_frames reader ~prefix ~total_length ~f =
  let rec loop () =
    let header = Bytes.create prefix in
    match%bind Reader.really_read reader header with
    | `Eof _ -> return ()
    | `Ok ->
      (match total_length (Bigstring.of_bytes header) with
       | None -> return ()
       | Some total when total < prefix -> return ()
       | Some total ->
         let rest = Bytes.create (total - prefix) in
         (match%bind Reader.really_read reader rest with
          | `Eof _ -> return ()
          | `Ok ->
            if f (Bytes.to_string header ^ Bytes.to_string rest)
            then loop ()
            else return ()))
  in
  loop ()
;;

let read_boe_frames reader ~f =
  read_frames
    reader
    ~prefix:4
    ~total_length:(fun b ->
      match Boe.frame_length b ~pos:0 with
      | Ok n when n >= Boe.header_length -> Some n
      | Ok _ | Error _ -> None)
    ~f
;;

let read_pitch_frames reader ~f =
  read_frames
    reader
    ~prefix:2
    ~total_length:(fun b -> Some (Pitch.Frame.frame_length b ~pos:0))
    ~f
;;

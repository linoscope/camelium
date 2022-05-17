open Core

let decode_string_int s =
  let bytes = s |> Bytes.of_string in
  match Bytes.length bytes with
  | 1 -> Ok (Caml.Bytes.get_int8 bytes 0)
  | 2 -> Ok (Caml.Bytes.get_int16_be bytes 0)
  | 4 -> Ok (Caml.Bytes.get_int32_be bytes 0 |> Int32.to_int_exn)
  | 8 -> begin
      match Caml.Bytes.get_int64_be bytes 0 |> Int64.to_int with
      | None   -> Error "Integer using 64 bits."
      | Some x -> Ok x
    end
  | n -> Error (Printf.sprintf "Invalid int byte length: %d" n)

let decode_string_int_exn s =
  decode_string_int s |> Result.ok_or_failwith

let inet_addr_of_bytes s =
  let decode_ipv4 s =
    s
    |> String.to_list
    |> List.map ~f:(fun c -> c |> Char.to_int |> Int.to_string)
    |> String.concat ~sep:"."
    |> Unix.Inet_addr.of_string
  in
  let decode_ipv6 s =
    let rec loop acc = function
      | [] ->
        acc
        |> List.rev
        |> List.map ~f:(Printf.sprintf "%x")
        |> String.concat ~sep:":"
        |> Unix.Inet_addr.of_string
      | x::y::r ->
        let part = (0x100 * Char.to_int x) + Char.to_int y in
        loop (part::acc) r
      | [_] -> assert false
    in
    loop [] (String.to_list s)
  in
  match String.length s with
  |  4 -> Ok (decode_ipv4 s)
  | 16 -> Ok (decode_ipv6 s)
  |  n -> Error (Printf.sprintf "Invalid IP bytes length: %d" n)

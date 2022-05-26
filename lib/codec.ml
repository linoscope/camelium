open Core
open Unix

(* TODO: Check if encoded int be larger than 63 bits. *)
let int_of_bytes s =
  let len = String.length s in
  if len > 8 then
    Error (Printf.sprintf "Invalid int byte length: %d" len)
  else
    Ok (
      s
      |> String.to_list
      |> List.fold ~init:0 ~f:(fun acc c -> acc * 0x100 + Char.to_int c)
    )

let bytes_of_int n =
  let rec loop acc n =
    if n = 0 then
      acc |> String.of_char_list
    else
      loop ((Char.of_int_exn (n % 0x100))::acc) (n / 0x100)
  in
  loop [] n


let int_of_bytes_exn s =
  int_of_bytes s |> Result.ok_or_failwith

let inet_addr_of_bytes s =
  let decode_ipv4 s =
    s
    |> String.to_list
    |> List.map ~f:(fun c -> c |> Char.to_int |> Int.to_string)
    |> String.concat ~sep:"."
    |> Inet_addr.of_string
  in
  let decode_ipv6 s =
    let rec loop acc = function
      | [] ->
        acc
        |> List.rev
        |> List.map ~f:(Printf.sprintf "%x")
        |> String.concat ~sep:":"
        |> Inet_addr.of_string
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

let bytes_of_inet_addr addr =
  let char_of_stringint s =
    (* e.g. "127" -> '\127' *)
    s |> Int.of_string |> Char.of_int_exn
  in
  let addr_str = addr |> Inet_addr.to_string in
  if String.(addr_str = "::1") then
    `Hex "00000000000000000000000000000001"
    |> Hex.to_string
  else
    addr_str
    |> String.split_on_chars ~on:['.'; ':']
    |> List.map ~f:char_of_stringint
    |> String.of_char_list

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
  let parts =
    s
    |> String.to_list
    |> List.map ~f:(fun c -> c |> Char.to_int |> Int.to_string)
  in
  if List.length parts = 4 then
    (* ipv4 *)
    parts
    |> String.concat ~sep:"."
    |> Unix.Inet_addr.of_string
  else
    (* parts *)
    (* |> String.concat ~sep:":" *)
    (* |> (fun s -> Stdio.printf "ip=%s\n" s; s) *)
    (* |> Unix.Inet_addr.of_string *)
    Unix.Inet_addr.localhost

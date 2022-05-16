open Core

let decode_string_int8 s =
  s
  |> Bytes.of_string
  |> (fun b -> Caml.Bytes.get_int8 b 0)

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

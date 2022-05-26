open Camelium
open Core
open Stdio

let print_hex_string s = s |> String.iter ~f:(fun c -> printf "\\x%02x" (Char.to_int c))

let%expect_test "test inet_addr_of_bytes" =
  Codec.inet_addr_of_bytes "\127\000\000\001"
  |> [%sexp_of : (Unix.Inet_addr.t, string) Result.t]
  |> print_s;

  [%expect {| (Ok 127.0.0.1) |}]


let%expect_test "test bytes_of_inet_addr" =
  let addr = Unix.Inet_addr.of_string "127.0.0.1" in
  Codec.bytes_of_inet_addr addr
  |> print_hex_string;

  [%expect {| \x7f\x00\x00\x01 |}]

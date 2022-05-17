open Core

val decode_string_int : string -> (int, string) Result.t

val decode_string_int_exn : string -> int

val inet_addr_of_bytes : string -> Unix.Inet_addr.t

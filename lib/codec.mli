open Core

(** [int_of_bytes s] decodes an [int] from a big-endian encoded bytes *)
val int_of_bytes : string -> (int, string) Result.t

(** Same as [int_of_bytes] but raises a [Failure] in case the decoding fails *)
val int_of_bytes_exn : string -> int

(** [bytes_of_int i] encodes [i] into big-endian bytes *)
val bytes_of_int : int -> string

(** [inet_addr_of_bytes s] decodes [Unix.Inet_addr.t] encoded in [s] *)
val inet_addr_of_bytes : string -> (Unix.Inet_addr.t, string) Result.t

(** [bytes_of_inet_addr addr] encodes [addr] into bytes *)
val bytes_of_inet_addr : Unix.Inet_addr.t -> string

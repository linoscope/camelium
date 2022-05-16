(** Ethereum Node Record V4. Specs described in EIP-778. *)
open Base

type t
[@@deriving sexp_of]

val decode : string -> (t, string) Result.t

val decode_and_verify : string -> (t, string) Result.t

(* val encode : t -> string *)

module For_tests : sig
  val dummy_node_record : t
end

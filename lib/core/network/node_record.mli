(** Ethereum Node Record V4. Specs described in EIP-778. *)

(* open Base *)

type t = {
  (* Name of identity scheme, e.g. "v4" *)
  id : string;
  (* The sequence number, a 64-bit unsigned integer. Nodes should increase the number whenever the record changes and republish the record. *)
  seq : int;
  (* Cryptographic signature of record contents *)
  signature : string;
  (* Arbitrary key/value pairs *)
  key_value_pairs : (string * string) list
}


(* val decode_and_verify : string -> (t, string) Result.t *)

(* val encode : t -> string *)

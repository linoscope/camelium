(** Ethereum Node Record V4. Specs described in EIP-778. *)

(* open! Base *)

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

(* let prefix = "enr:" *)

(* let decode_and_verify s = *)
(*   match String.chop_prefix s ~prefix with *)
(*   | None -> Error "Invalid prefix" *)
(*   | Some s -> *)
(*     match Rlp.decode s with *)
(*     | `Rlp_data _ *)
(*     | `Rlp_list [_] *)
(*     | `Rlp_list [_; _] -> Error "Invalid data" *)
(*     | `Rlp_list (signature::seq::rest) -> *)
(*       let decode_key_value_pair = function *)
(*         | `Rlp_list [`Rlp_data k; `Rlp_data v] -> Some (k, v) *)
(*         | `Rlp_list _ | `Rlp_data _ -> None *)
(*       in *)

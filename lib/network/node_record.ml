(** Ethereum Node Record V4. Specs described in EIP-778. *)

type t = {
  id : string;             (* name of identity scheme, e.g. "v4" *)
  secp256k1 : string;      (* compressed secp256k1 public key, 33 bytes *)
}

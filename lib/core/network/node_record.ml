(** Ethereum Node Record V4. Specs described in EIP-778. *)

open! Base

type t = {
  (* The sequence number, a 64-bit unsigned integer. Nodes should increase the number whenever the record changes and republish the record. *)
  seq : int;
  (* Cryptographic signature of record contents *)
  signature : string;
  (* Arbitrary key/value pairs *)
  key_value_pairs : (string * string) list
}
[@@deriving sexp_of]

let prefix = "enr:"

let decode_raw s =
  let decoded = Rlp.decode s in
  match decoded with
  | Rlp_data _
  | Rlp_list []
  | Rlp_list [_]
  | Rlp_list [_; _] ->
    let msg = Printf.sprintf "RLP length too short: %s" (decoded |> Rlp.sexp_of_item |> Sexp.to_string) in
    Error msg
  | Rlp_list (signature::seq::rest) ->
    match signature, seq with
    | Rlp_list _, _ -> Error "Invalid signature format"
    | _, Rlp_list _ -> Error "Invalid seq format"
    | Rlp_data signature, Rlp_data seq ->
      let rec decode_pairs acc = function
        | []                                            -> List.rev acc
        | (Rlp.Rlp_data k)::(Rlp_data v)::l             -> decode_pairs ((k, v)::acc) l
        | [_] | (Rlp_list _::_::_) | (_::Rlp_list _::_) -> raise Caml.Exit
      in
      try
        let key_value_pairs = decode_pairs [] rest in
        let seq = Char.to_int seq.[0] in
        Ok { seq; signature; key_value_pairs}
      with
      | Caml.Exit -> Error "Invalid key value pair format"

let decode s =
  match String.chop_prefix s ~prefix with
  | None -> Error "Invalid prefix"
  | Some s ->
    let raw = Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet s in
    decode_raw raw

let find (pairs : (string * string) list) (key : string) : string option =
  List.Assoc.find ~equal:String.equal pairs key

let verify t =
  let {seq; signature; key_value_pairs = pairs} = t in
  match find pairs "secp256k1" with
  | None -> Error "secp256k1 field missing"
  | Some public_key ->
    let rlp_pairs = pairs |> List.bind ~f:(fun (k, v) -> Rlp.[Rlp_data k; Rlp_data v]) in
    let rlp_seq = Rlp.Rlp_data (Int.to_string seq) in
    let encoded_content = Rlp.encode (Rlp_list (rlp_seq::rlp_pairs)) in
    Secp256k1.verify ~pk:public_key ~msg:encoded_content ~signature

let decode_and_verify s =
  let open Result.Let_syntax in
  decode s >>= fun decoded ->
  verify decoded >>= fun verified ->
  if verified then
    Ok decoded
  else
    Error "Verification failed"

module For_tests = struct
  let dummy_node_record = { signature = "aaa"; seq = 0; key_value_pairs = []}
end

open! Core

type packet_header = {
  hash        : string;      (* 32 bytes. keccak256(signature||protocol_type||packet_data)  *)
  signature   : string;      (* 65 bytes. sign(packet_type||packet_data) *)
  packet_type : packet_type; (*  1 byte *)
}
and packet_type = Ping_type | Pong_type | Find_node_type | Neighbors_type | Enr_request_type | Enr_response_type [@@deriving sexp_of]

type packet_data =
  | Ping of {
      version    : int;
      from       : endpoint;
      to_        : endpoint;
      expiration : int; (* epoch seconds *)
      enr_seq    : int option;
    }
  | Pong of {
      to_       : endpoint;
      ping_hash : string;
      enr_seq   : int option;
    }
  | Find_node of {
      target     : string; (* 64-byte secp256k1 public key of the target. *)
      expiration : int;    (* epoch seconds *)
    }
  | Neighbors of {
      expiration : int;
      nodes : node list;
    }
  | Enr_request of {
      expiration : int;
    }
  | Enr_response of {
      request_hash : string;
      node_record  : Node_record.t;
    }
and endpoint = {
  ip : Unix.Inet_addr.t;
  udp_port : int;
  tcp_port : int
}
and node = {
  node_id : string;
  endpoint : endpoint;
}
[@@deriving sexp_of]

type t = {
  packet_header : packet_header;
  packet_data   : packet_data;
}


let decode s =
  let open Result.Let_syntax in
  let decode_packet_type : char -> (packet_type, string) Result.t = function
    | '\x01' -> Ok Ping_type
    | '\x02' -> Ok Pong_type
    | '\x03' -> Ok Find_node_type
    | '\x04' -> Ok Neighbors_type
    | '\x05' -> Ok Enr_request_type
    | '\x06' -> Ok Enr_response_type
    | n -> Error (Printf.sprintf "Invalid packet type byte: %c" n)
  in
  let decode_endpoint_rlp : Rlp.item list -> (endpoint, string) Result.t = function
    | (Rlp_data ip)::(Rlp_data udp_port)::(Rlp_data tcp_port)::[] ->
      let%bind udp_port = Util.decode_string_int udp_port in
      let%bind tcp_port = Util.decode_string_int tcp_port in
      let ip = Util.inet_addr_of_bytes ip in
      Ok { ip; udp_port; tcp_port; }
    | _ -> Error "Invalid endpoint format"
  in
  let decode_enr_seq_rlp : Rlp.item list -> int option = function
    | (Rlp_data x)::_ -> begin match Util.decode_string_int x with
        | Ok x    -> Some x
        | Error _ -> None
      end
    | [] | _::_ -> None
  in
  let open Result.Let_syntax in
  let _hash = String.sub s ~pos:0 ~len:32 in
  let _signature = String.sub s ~pos:32 ~len:65 in   (* TODO: Recover public key from signature *)
  let%bind packet_type = decode_packet_type s.[97] in
  let packet_data_rlp = String.sub s ~pos:98 ~len:(String.length s - 98) |> Rlp.decode in
  match packet_type, packet_data_rlp with
  | Ping_type, Rlp_list ((Rlp_data version)::(Rlp_list from_items)::(Rlp_list to_items)::(Rlp_data expiration)::rest) -> begin
      let%bind version = Util.decode_string_int version in
      let%bind from = decode_endpoint_rlp from_items in
      let%bind to_  = decode_endpoint_rlp to_items in
      let%bind expiration = Util.decode_string_int expiration in
      let enr_seq = decode_enr_seq_rlp rest in
      Ok (Ping {version; from; to_; expiration; enr_seq })
    end
  | Pong_type, Rlp_list ((Rlp_list to_items)::(Rlp_data ping_hash)::rest) -> begin
      let%bind to_  = decode_endpoint_rlp to_items in
      let enr_seq = decode_enr_seq_rlp rest in
      Ok (Pong { to_; ping_hash; enr_seq })
    end
  | packet_type, rlp_item ->
    Error (Printf.sprintf "Invalid packet data. Packet type:%s, RLP=%s"
             (sexp_of_packet_type packet_type |> Sexp.to_string)
             (Rlp.sexp_of_item rlp_item |> Sexp.to_string))

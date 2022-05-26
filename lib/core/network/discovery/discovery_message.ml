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
[@@deriving sexp_of]


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
      let%bind udp_port = Codec.int_of_bytes udp_port in
      let%bind tcp_port = Codec.int_of_bytes tcp_port in
      let%bind ip = Codec.inet_addr_of_bytes ip in
      Ok { ip; udp_port; tcp_port; }
    | _ -> Error "Invalid endpoint format"
  in
  let decode_enr_seq_rlp : Rlp.item list -> int option = function
    | (Rlp_data x)::_ -> begin match Codec.int_of_bytes x with
        | Ok x    -> Some x
        | Error _ -> None
      end
    | [] | _::_ -> None
  in
  let rec decode_nodes : Rlp.item list -> (node list, string) Result.t = function
    | [] -> Ok []
    | (Rlp_list (x::y::z::(Rlp_data node_id)::[]))::rest ->
      let%bind endpoint = decode_endpoint_rlp [x; y; z] in
      let%bind nodes = decode_nodes rest in
      Ok ({node_id; endpoint}::nodes)
    | rlps -> Error (Printf.sprintf "Invalid nodes format: %s" (rlps |> [%sexp_of : Rlp.item list] |> Sexp.to_string_hum))
  in
  let hash = String.sub s ~pos:0 ~len:32 in
  let signature = String.sub s ~pos:32 ~len:65 in   (* TODO: Recover public key from signature *)
  let%bind packet_type = decode_packet_type s.[97] in
  let packet_header = {hash; signature; packet_type} in
  let packet_data_rlp = String.sub s ~pos:98 ~len:(String.length s - 98) |> Rlp.decode in
  let%bind packet_data =
    match packet_type, packet_data_rlp with
    | Ping_type, Rlp_list ((Rlp_data version)::(Rlp_list from_items)::(Rlp_list to_items)::(Rlp_data expiration)::rest) ->
      let%bind version = Codec.int_of_bytes version in
      let%bind from = decode_endpoint_rlp from_items in
      let%bind to_  = decode_endpoint_rlp to_items in
      let%bind expiration = Codec.int_of_bytes expiration in
      let enr_seq = decode_enr_seq_rlp rest in
      Ok (Ping {version; from; to_; expiration; enr_seq })
    | Pong_type, Rlp_list ((Rlp_list to_items)::(Rlp_data ping_hash)::rest) ->
      let%bind to_  = decode_endpoint_rlp to_items in
      let enr_seq = decode_enr_seq_rlp rest in
      Ok (Pong { to_; ping_hash; enr_seq })
    | Find_node_type, Rlp_list ((Rlp_data target)::(Rlp_data expiration)::_) ->
      let%bind expiration = Codec.int_of_bytes expiration in
      Ok (Find_node { target; expiration })
    | Neighbors_type, Rlp_list ((Rlp_list node_rlps)::(Rlp_data expiration)::_) ->
      let%bind nodes = decode_nodes node_rlps in
      let%bind expiration = Codec.int_of_bytes expiration in
      Ok (Neighbors {nodes; expiration})
    | Enr_request_type, Rlp_list ((Rlp_data expiration)::_) ->
      let%bind expiration = Codec.int_of_bytes expiration in
      Ok (Enr_request {expiration})
    | Enr_response_type, Rlp_list ((Rlp_data request_hash)::node_record_rlp::_) ->
      let%bind node_record = Node_record.decode_rlp node_record_rlp in
      Ok (Enr_response {request_hash; node_record})
    | packet_type, rlp_item ->
      Error (Printf.sprintf "Invalid packet data. Packet type:%s, RLP=%s"
               (sexp_of_packet_type packet_type |> Sexp.to_string)
               (Rlp.sexp_of_item rlp_item |> Sexp.to_string))
  in
  Ok {packet_header; packet_data}

let encode packet_data =
  let encode_endpoint ({ip; udp_port; tcp_port} : endpoint) : Rlp.item =
    Rlp_list [
      Rlp_data (Codec.bytes_of_inet_addr ip);
      Rlp_data (Codec.bytes_of_int udp_port);
      Rlp_data (Codec.bytes_of_int tcp_port);
    ]
  in
  let encode_nodes (nodes : node list) : Rlp.item =
    nodes
    |> List.bind ~f:(fun { node_id = _; endpoint = { ip; udp_port; tcp_port} } ->
        Rlp.[ Rlp_data (Codec.bytes_of_inet_addr ip);
              Rlp_data (Codec.bytes_of_int udp_port);
              Rlp_data (Codec.bytes_of_int tcp_port) ])
    |> (fun l -> Rlp.Rlp_list l)
  in
  let rlp =
    match packet_data with
    | Ping { version; from; to_; expiration; enr_seq } ->
      let version = Rlp.item_of_int version in
      let from = encode_endpoint from in
      let to_ = encode_endpoint to_ in
      let expiration = Rlp.Rlp_data (Codec.bytes_of_int expiration) in
      begin match enr_seq with
        | None -> Rlp.Rlp_list [version; from; to_; expiration]
        | Some enr_seq ->
          let enr_seq = Rlp.Rlp_data (Codec.bytes_of_int enr_seq) in
          Rlp.Rlp_list [version; from; to_; expiration; enr_seq]
      end
    | Pong { to_; ping_hash; enr_seq } ->
      let to_ = encode_endpoint to_ in
      begin match enr_seq with
        | None -> Rlp.Rlp_list [to_; Rlp_data ping_hash]
        | Some enr_seq ->
          let enr_seq = Rlp.Rlp_data (Codec.bytes_of_int enr_seq) in
          Rlp.Rlp_list [to_; Rlp_data ping_hash; enr_seq]
      end
    | Find_node { target; expiration } ->
      let expiration = Rlp.Rlp_data (Codec.bytes_of_int expiration) in
      Rlp_list [Rlp_data target; expiration]
    | Neighbors { nodes; expiration } ->
      let expiration = Rlp.Rlp_data (Codec.bytes_of_int expiration) in
      Rlp.Rlp_list [encode_nodes nodes; expiration]
    | Enr_request { expiration } ->
      let expiration = Rlp.Rlp_data (Codec.bytes_of_int expiration) in
      Rlp.Rlp_list [expiration]
    | Enr_response { request_hash; node_record } ->
      Rlp.Rlp_list [Rlp_data request_hash; Node_record.encode_rlp node_record]
  in
  Rlp.encode rlp

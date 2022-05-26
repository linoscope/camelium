(** Node discovery messages.
    See {{:https://github.com/ethereum/devp2p/blob/master/discv4.md}[discv4.md]} for more information. *)

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
  ip : Unix.inet_addr;
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

val decode : string -> (t, string) Result.t

val encode : packet_data -> string

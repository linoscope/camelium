(** Nodes managed in Kademlia peer discovery.  *)

type t [@@deriving sexp_of]

val create : Node_record.t -> Kademlia_id.t -> t

val get_node_record : t -> Node_record.t

val get_kademlia_id : t -> Kademlia_id.t

(** [distance t1 t2] returns distance between the kademlia id of t1 and t2 *)
val distance : t -> t -> int

val equal : t -> t -> bool

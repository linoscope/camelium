(** Node table in Kademlia peer discovery.
    Implemented using "K-Bucket"s where each K-bucket holds up-to k kadmelia nodes. *)

type t
[@@deriving sexp_of]

val create : Kademlia_node.t -> int -> t

type add_result =
  | Already_existed
  | Added
  | Self
  | Bucket_full of Kademlia_node.t (* Eviction candidate. *)
[@@deriving sexp_of]

val add : t -> Kademlia_node.t -> add_result

val evict : t -> Kademlia_node.t -> unit

(** [nearest t ~target ~limit] returns top [limit] nodes that are close to [target] node *)
val nearest_nodes : t -> target:Kademlia_node.t -> limit:int -> Kademlia_node.t list

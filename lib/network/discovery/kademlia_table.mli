(** Node table in Kademlia peer discovery.
    Quote from {{:https://github.com/ethereum/devp2p/blob/master/discv4.md}[discv4.md]}:
    > Nodes in the Discovery Protocol keep information about other nodes in their neighborhood. Neighbor nodes are stored in a routing table consisting of 'k-buckets'. For each 0 ≤ i < 256, every node keeps a k-bucket for nodes of distance between 2i and 2i+1 from itself. The Node Discovery Protocol uses k = 16, i.e. every k-bucket contains up to 16 node entries. The entries are sorted by time last seen — least-recently seen node at the head, most-recently seen at the tail. *)

type t
[@@deriving sexp_of]

(** [create ~k self_node] creates a new kademlia table.
    [k] is the size of each k-bucket. It is an optional argumant that defaults to 16.
    [self_node] is the node of where this table is stored *)
val create : ?k:int -> Kademlia_node.t -> t

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

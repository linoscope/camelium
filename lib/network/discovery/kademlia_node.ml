open! Core

type t = {
  node_record : (Node_record.t [@sexp.opaque]);
  kademlia_id : Kademlia_id.t;
}
[@@deriving sexp_of]


let create node_record kademlia_id = { node_record; kademlia_id }

let get_node_record t = t.node_record

let get_kademlia_id t = t.kademlia_id

let distance t1 t2 = Kademlia_id.distance t1.kademlia_id t2.kademlia_id

let equal t1 t2 = Kademlia_id.equal t1.kademlia_id t2.kademlia_id

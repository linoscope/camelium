open Core

(* TODO: Think about when we should "touch" the node. *)

type t = {
  self_node : Kademlia_node.t;
  k         : int;
  kbuckets  : Kademlia_node.t Doubly_linked.t array;
}
[@@deriving sexp_of]

let create ?(k = 16) self_node = {
  self_node;
  k;
  kbuckets = Array.init 160 ~f:(fun _ -> Doubly_linked.create ());
}

type add_result =
  | Already_existed
  | Added
  | Self
  | Bucket_full of [`Eviction_candidate of Kademlia_node.t]
[@@deriving sexp_of]

let add t node =
  let distance = Kademlia_node.distance t.self_node node in
  let kbucket = t.kbuckets.(distance) in
  if Doubly_linked.mem kbucket node ~equal:Kademlia_node.equal then
    Already_existed
  else if Kademlia_node.(equal node t.self_node) then
    Self
  else if Doubly_linked.length kbucket < t.k then begin
    (* Head contains the "most recently seen" node *)
    ignore @@ Doubly_linked.insert_first kbucket node;
    Added
  end else
    (* Tail contains the "least recently seen" node *)
    let eviction_candidate = Doubly_linked.last kbucket |> Option.value_exn in
    Bucket_full (`Eviction_candidate eviction_candidate)

let evict t node =
  let distance = Kademlia_node.distance t.self_node node in
  let kbucket = t.kbuckets.(distance) in
  Doubly_linked.filter_inplace kbucket ~f:(fun n -> not (Kademlia_node.equal n node))

let nearest_nodes t ~target ~limit =
  t.kbuckets
  |> Array.to_list
  |> List.bind ~f:Doubly_linked.to_list
  |> List.sort ~compare:(fun n1 n2 ->
      Int.compare
        Kademlia_node.(distance target n1)
        Kademlia_node.(distance target n2))
  |> (fun l -> List.take l limit)

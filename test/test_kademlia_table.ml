open! Camelium
open! Core
open! Stdio

let create_node s =
  let node_record = Node_record.{ id = "v4"; signature = "aaa"; seq = 0; key_value_pairs = []} in
  Kademlia_node.create node_record Kademlia_id.(of_string s)

let%expect_test "Adding self_node" =
  let self_node = create_node "\x00\x00" in
  let t = Kademlia_table.create ~k:3 self_node in

  Kademlia_table.add t self_node
  |> Kademlia_table.sexp_of_add_result
  |> print_s;
  [%expect {| Self |}]

let%expect_test "Adding duplicate node" =
  let self_node = create_node "\x00\x00" in
  let t = Kademlia_table.create ~k:3 self_node in

  let node = create_node "\x00\x04" in
  ignore @@ Kademlia_table.add t node;
  Kademlia_table.add t node
  |> Kademlia_table.sexp_of_add_result
  |> print_s;
  [%expect {| Already_existed |}]

let%expect_test "Can add up-to k nodes in same bucket" =
  let self_node = create_node "\x00\x00" in
  let t = Kademlia_table.create ~k:3 self_node in

  (* Add two nodes that fall into the same k-bucket  *)
  ignore @@ Kademlia_table.add t (create_node "\x00\x04"); (* 0b00001000 *)
  ignore @@ Kademlia_table.add t (create_node "\x00\x05"); (* 0b00001001 *)

  (* Adding a third element to the same k-bucket should succeed without issue *)
  Kademlia_table.add t (create_node "\x00\x06") (* 0b00001010 *)
  |> Kademlia_table.sexp_of_add_result
  |> print_s;
  [%expect {| Added |}];

  (* Adding a forth element to the same k-bucket should result in Bucket_full *)
  Kademlia_table.add t (create_node "\x00\x07") (* 0b00001011 *)
  |> Kademlia_table.sexp_of_add_result
  |> print_s;
  (* returned eviction_candidate is the first node we added. *)
  [%expect {| (Bucket_full ((node_record <opaque>) (kademlia_id "\\x00\\x04"))) |}]

let%expect_test "nearest_nodes returns nearest nodes" =
  let self_node = create_node "\x00\x00" in
  let t = Kademlia_table.create ~k:3 self_node in
  ignore @@ Kademlia_table.add t (create_node "\x00\x04"); (* 0b00000100 *)
  ignore @@ Kademlia_table.add t (create_node "\x00\x01"); (* 0b00000001 *)
  ignore @@ Kademlia_table.add t (create_node "\x00\x02"); (* 0b00000010 *)
  ignore @@ Kademlia_table.add t (create_node "\x00\x03"); (* 0b00000011 *)

  t
  |> Kademlia_table.nearest_nodes ~target:self_node ~limit:3
  |> [%sexp_of : Kademlia_node.t list]
  |> print_s;
  [%expect {|
    (((node_record <opaque>) (kademlia_id "\\x00\\x01"))
     ((node_record <opaque>) (kademlia_id "\\x00\\x03"))
     ((node_record <opaque>) (kademlia_id "\\x00\\x02"))) |}]

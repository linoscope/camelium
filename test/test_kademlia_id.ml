open Camelium
open! Base
open! Stdio

let%expect_test "test distance 0" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x00\x00" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 0 |}]

let%expect_test "test distance 1" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x00\x01" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 1 |}]

let%expect_test "test distance 2" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x00\x02" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 2 |}]

let%expect_test "test distance 2 (different least significant bit)" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x00\x03" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 2 |}]

let%expect_test "test distance 3" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x00\x04" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 3 |}]

let%expect_test "test distance 9" =
  let t1 = Kademlia_id.of_string "\x00\x00" in
  let t2 = Kademlia_id.of_string "\x01\x00" in

  Kademlia_id.distance t1 t2 |> printf "%d";

  [%expect {| 9 |}]

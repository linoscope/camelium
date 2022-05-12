open Camelium
open Base
open Stdio

let print_hex_string s = s |> String.iter ~f:(fun c -> printf "\\x%02x" (Char.to_int c))

let%expect_test "encode int" =
  Rlp.encode_int 0x123456 |> print_hex_string;
  [%expect {| \x12\x34\x56 |}]   (* big endian *)

let%expect_test "encode empty string" =
  Rlp.encode_string "" |> print_hex_string;
  [%expect {| \x80 |}]

let%expect_test "encode single byte in [0x00, 0x79]" =
  Rlp.encode_string "\x00" |> print_hex_string;
  [%expect {| \x00 |}];
  Rlp.encode_string "\x79" |> print_hex_string;
  [%expect {| \x79 |}]

let%expect_test "encode single byte in [0x80, 0xff]" =
  Rlp.encode_string "\x80" |> print_hex_string;
  [%expect {| \x81\x80 |}];
  Rlp.encode_string "\xff" |> print_hex_string;
  [%expect {| \x81\xff |}]

let%expect_test "encode [2, 55] long string " =
  Rlp.encode_string "hello world" |> print_hex_string;
  [%expect {| \x8b\x68\x65\x6c\x6c\x6f\x20\x77\x6f\x72\x6c\x64 |}];

  (* 55 chars *)
  Rlp.encode_string "Lorem ipsum dolor sit amet, consectetur adipisicing eli" |> print_hex_string;
  [%expect {| \xb7\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69 |}]

let%expect_test "encode [56,] long string" =
  (* 56 chars *)
  Rlp.encode_string "Lorem ipsum dolor sit amet, consectetur adipisicing elit" |> print_hex_string;
  [%expect {| \xb8\x38\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69\x74 |}]

let%expect_test "encode empty list" =
  Rlp.encode (`Rlp_list []) |> print_hex_string;
  [%expect {| \xc0 |}]

let%expect_test "encode [1, 55] long list" =
  Rlp.encode (`Rlp_list [`Rlp_data "dog"; `Rlp_data "god"; `Rlp_data "cat"]) |> print_hex_string;
  [%expect {| \xcc\x83\x64\x6f\x67\x83\x67\x6f\x64\x83\x63\x61\x74 |}];

  (* 55 bytes *)
  Rlp.encode (`Rlp_list [
      `Rlp_data "asdf";
      `Rlp_data "qwer";
      `Rlp_data "zxcv";
      `Rlp_data "asdf";
      `Rlp_data "qwer";
      `Rlp_data "zxcv";
      `Rlp_data "asdf";
      `Rlp_data "qwer";
      `Rlp_data "zxcv";
      `Rlp_data "asdf";
      `Rlp_data "qwer"])
  |> print_hex_string;
  [%expect {| \xf7\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72 |}]

let%expect_test "encode [56,] long list" =
  Rlp.encode (
    `Rlp_list [
      `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
      `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
      `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
      `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
    ])
  |> print_hex_string;
  [%expect {| \xf8\x40\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76 |}]


let%expect_test "dencode int" =
  Rlp.decode_int "\x12\x34\x56" |> Stdio.printf "%x";
  [%expect {| 123456 |}]

let test_decode string_to_decode expected_item =
  let actual = Rlp.decode string_to_decode in
  Rlp.equal_item actual expected_item


let%test "decode empty string" =
  test_decode "\x80" (`Rlp_data "")

let%test "decode single byte in [0x00, 0x79]" =
  test_decode "\x79" (`Rlp_data "\x79")

let%test "decode single byte in [0x80, 0xff]" =
  test_decode "\x81\xff" (`Rlp_data "\xff")

let%test "decode [2, 55] long string " =
  test_decode
    "\xb7\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69"
    (`Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing eli")

let%test "decode [56,] long string" =
  (* 56 chars *)
  test_decode
    "\xb8\x38\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69\x74"
    (`Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing elit")

let%test "decode empty list" =
  test_decode
    "\xc0"
    (`Rlp_list [])

let%test "decode [1, 55] long list" =
  test_decode
    "\xcc\x83\x64\x6f\x67\x83\x67\x6f\x64\x83\x63\x61\x74"
    (`Rlp_list [`Rlp_data "dog"; `Rlp_data "god"; `Rlp_data "cat"])

let%test "decode [56,] long list" =
  test_decode
    "\xf8\x40\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76"
    (`Rlp_list [
        `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
        `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
        `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];
        `Rlp_list [`Rlp_data "asdf"; `Rlp_data "qwer";`Rlp_data "zxcv"];])

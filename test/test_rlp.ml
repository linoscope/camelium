open Camelium
open Core
open Stdio

let print_hex_string s = s |> String.iter ~f:(fun c -> printf "\\x%02x" (Char.to_int c))

let%expect_test "encode empty string" =
  Rlp.encode (Rlp_data "") |> print_hex_string;
  [%expect {| \x80 |}]

let%expect_test "encode single byte in [0x00, 0x79]" =
  Rlp.encode (Rlp_data "\x00") |> print_hex_string;
  [%expect {| \x00 |}];
  Rlp.encode (Rlp_data "\x79") |> print_hex_string;
  [%expect {| \x79 |}]

let%expect_test "encode single byte in [0x80, 0xff]" =
  Rlp.encode (Rlp_data "\x80") |> print_hex_string;
  [%expect {| \x81\x80 |}];
  Rlp.encode (Rlp_data "\xff") |> print_hex_string;
  [%expect {| \x81\xff |}]

let%expect_test "encode [2, 55] long string " =
  Rlp.encode (Rlp_data "hello world") |> print_hex_string;
  [%expect {| \x8b\x68\x65\x6c\x6c\x6f\x20\x77\x6f\x72\x6c\x64 |}];

  (* 55 chars *)
  Rlp.encode (Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing eli") |> print_hex_string;
  [%expect {| \xb7\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69 |}]

let%expect_test "encode [56,] long string" =
  (* 56 chars *)
  Rlp.encode (Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing elit") |> print_hex_string;
  [%expect {| \xb8\x38\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69\x74 |}]

let%expect_test "encode empty list" =
  Rlp.encode (Rlp_list []) |> print_hex_string;
  [%expect {| \xc0 |}]

let%expect_test "encode [1, 55] long list" =
  Rlp.encode (Rlp_list [Rlp_data "dog"; Rlp_data "god"; Rlp_data "cat"]) |> print_hex_string;
  [%expect {| \xcc\x83\x64\x6f\x67\x83\x67\x6f\x64\x83\x63\x61\x74 |}];

  (* 55 bytes *)
  Rlp.encode (Rlp_list [
      Rlp_data "asdf";
      Rlp_data "qwer";
      Rlp_data "zxcv";
      Rlp_data "asdf";
      Rlp_data "qwer";
      Rlp_data "zxcv";
      Rlp_data "asdf";
      Rlp_data "qwer";
      Rlp_data "zxcv";
      Rlp_data "asdf";
      Rlp_data "qwer"])
  |> print_hex_string;
  [%expect {| \xf7\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72 |}]

let%expect_test "encode [56,] long list" =
  Rlp.encode (
    Rlp_list [
      Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
      Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
      Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
      Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
    ])
  |> print_hex_string;
  [%expect {| \xf8\x40\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76 |}]

let test_decode string_to_decode expected_item =
  let actual = Rlp.decode string_to_decode in
  Rlp.equal_item actual expected_item


let%test "decode empty string" =
  test_decode "\x80" (Rlp_data "")

let%test "decode single byte in [0x00, 0x79]" =
  test_decode "\x79" (Rlp_data "\x79")

let%test "decode single byte in [0x80, 0xff]" =
  test_decode "\x81\xff" (Rlp_data "\xff")

let%test "decode [2, 55] long string " =
  test_decode
    "\xb7\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69"
    (Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing eli")

let%test "decode [56,] long string" =
  (* 56 chars *)
  test_decode
    "\xb8\x38\x4c\x6f\x72\x65\x6d\x20\x69\x70\x73\x75\x6d\x20\x64\x6f\x6c\x6f\x72\x20\x73\x69\x74\x20\x61\x6d\x65\x74\x2c\x20\x63\x6f\x6e\x73\x65\x63\x74\x65\x74\x75\x72\x20\x61\x64\x69\x70\x69\x73\x69\x63\x69\x6e\x67\x20\x65\x6c\x69\x74"
    (Rlp_data "Lorem ipsum dolor sit amet, consectetur adipisicing elit")

let%test "decode empty list" =
  test_decode
    "\xc0"
    (Rlp_list [])

let%test "decode [1, 55] long list" =
  test_decode
    "\xcc\x83\x64\x6f\x67\x83\x67\x6f\x64\x83\x63\x61\x74"
    (Rlp_list [Rlp_data "dog"; Rlp_data "god"; Rlp_data "cat"])

let%test "decode [56,] long list" =
  test_decode
    "\xf8\x40\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76\xcf\x84\x61\x73\x64\x66\x84\x71\x77\x65\x72\x84\x7a\x78\x63\x76"
    (Rlp_list [
        Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
        Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
        Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];
        Rlp_list [Rlp_data "asdf"; Rlp_data "qwer";Rlp_data "zxcv"];])

let%expect_test "decodes ethereum node record" =
  "\xf8\x84\xb8\x40\x70\x98\xad\x86\x5b\x00\xa5\x82\x05\x19\x40\xcb\x9c\xf3\x68\x36\x57\x24\x11\xa4\x72\x78\x78\x30\x77\x01\x15\x99\xed\x5c\xd1\x6b\x76\xf2\x63\x5f\x4e\x23\x47\x38\xf3\x08\x13\xa8\x9e\xb9\x13\x7e\x3e\x3d\xf5\x26\x6e\x3a\x1f\x11\xdf\x72\xec\xf1\x14\x5c\xcb\x9c\x01\x82\x69\x64\x82\x76\x34\x82\x69\x70\x84\x7f\x00\x00\x01\x89\x73\x65\x63\x70\x32\x35\x36\x6b\x31\xa1\x03\xca\x63\x4c\xae\x0d\x49\xac\xb4\x01\xd8\xa4\xc6\xb6\xfe\x8c\x55\xb7\x0d\x11\x5b\xf4\x00\x76\x9c\xc1\x40\x0f\x32\x58\xcd\x31\x38\x83\x75\x64\x70\x82\x76\x5f"
  |> Rlp.decode
  |> Rlp.sexp_of_item
  |> print_s;
  [%expect {|
    (Rlp_list
     ((Rlp_data
       "p\152\173\134[\000\165\130\005\025@\203\156\243h6W$\017\164rxx0w\001\021\153\237\\\209kv\242c_N#G8\243\b\019\168\158\185\019~>=\245&n:\031\017\223r\236\241\020\\\203\156")
      (Rlp_data "\001") (Rlp_data id) (Rlp_data v4) (Rlp_data ip)
      (Rlp_data "\127\000\000\001") (Rlp_data secp256k1)
      (Rlp_data
       "\003\202cL\174\rI\172\180\001\216\164\198\182\254\140U\183\r\017[\244\000v\156\193@\0152X\20518")
      (Rlp_data udp) (Rlp_data v_))) |}]

let%expect_test "decodes ping packet" =
  Hex.to_string (`Hex "71dbda3a79554728d4f94411e42ee1f8b0d561c10e1e5f5893367948c6a7d70bb87b235fa28a77070271b6c164a2dce8c7e13a5739b53b5e96f2e5acb0e458a02902f5965d55ecbeb2ebb6cabb8b2b232896a36b737666c55265ad0a68412f250001ea04cb847f000001820cfa8215a8d790000000000000000000000000000000018208ae820d058443b9a355")
  |> (fun s -> String.drop_prefix s 98) (* drop header since it is not RLP encoded *)
  |> Rlp.decode
  |> Rlp.sexp_of_item
  |> print_s;
  [%expect {|
    (Rlp_list
     ((Rlp_data "\004")
      (Rlp_list
       ((Rlp_data "\127\000\000\001") (Rlp_data "\012\250")
        (Rlp_data "\021\168")))
      (Rlp_list
       ((Rlp_data
         "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001")
        (Rlp_data "\b\174") (Rlp_data "\r\005")))
      (Rlp_data "C\185\163U"))) |}]

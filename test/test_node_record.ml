open Camelium
open! Base
open! Stdio

let%expect_test "decode example in EIP-778" =
  let encoded_record = "enr:-IS4QHCYrYZbAKWCBRlAy5zzaDZXJBGkcnh4MHcBFZntXNFrdvJjX04jRzjzCBOonrkTfj499SZuOh8R33Ls8RRcy5wBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQPKY0yuDUmstAHYpMa2_oxVtw0RW_QAdpzBQA8yWM0xOIN1ZHCCdl8" in
  Node_record.decode encoded_record
  |> [%sexp_of : (Node_record.t, string) Result.t]
  |> print_s;
  [%expect {|
    (Ok
     ((seq 1)
      (signature
       "p\152\173\134[\000\165\130\005\025@\203\156\243h6W$\017\164rxx0w\001\021\153\237\\\209kv\242c_N#G8\243\b\019\168\158\185\019~>=\245&n:\031\017\223r\236\241\020\\\203\156")
      (key_value_pairs
       ((id v4) (ip "\127\000\000\001")
        (secp256k1
         "\003\202cL\174\rI\172\180\001\216\164\198\182\254\140U\183\r\017[\244\000v\156\193@\0152X\20518")
        (udp v_))))) |}]

(* TODO: Pass this test. *)
let%expect_test "decode and verify in EIP-778" =
  let encoded_record = "enr:-IS4QHCYrYZbAKWCBRlAy5zzaDZXJBGkcnh4MHcBFZntXNFrdvJjX04jRzjzCBOonrkTfj499SZuOh8R33Ls8RRcy5wBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQPKY0yuDUmstAHYpMa2_oxVtw0RW_QAdpzBQA8yWM0xOIN1ZHCCdl8" in
  Node_record.decode_and_verify encoded_record
  |> [%sexp_of : (Node_record.t, string) Result.t]
  |> print_s;
  [%expect {|
    (Error "Verification failed") |}]

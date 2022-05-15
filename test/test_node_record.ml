open Camelium
open! Base
open! Stdio

let%expect_test "test example in EIP-778" =
  let encoded_record = "enr:-IS4QHCYrYZbAKWCBRlAy5zzaDZXJBGkcnh4MHcBFZntXNFrdvJjX04jRzjzCBOonrkTfj499SZuOh8R33Ls8RRcy5wBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQPKY0yuDUmstAHYpMa2_oxVtw0RW_QAdpzBQA8yWM0xOIN1ZHCCdl8" in
  Node_record.decode_and_verify encoded_record
  |> [%sexp_of : (Node_record.t, string) Result.t]
  |> print_s

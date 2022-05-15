type item =
  | Rlp_data of string
  | Rlp_list of item list
[@@deriving equal]

val encode_int : int -> string

val encode : item -> string

val encode_string : string -> string

val decode_int : string -> int

val decode : string -> item

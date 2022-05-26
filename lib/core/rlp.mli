type item =
  | Rlp_data of string
  | Rlp_list of item list
[@@deriving equal, sexp_of]

val encode : item -> string

val item_of_int : int -> item

val decode : string -> item

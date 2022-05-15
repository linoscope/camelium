(** ID used in Kademlia peer discovery. *)

type t [@@deriving sexp_of]

val of_string : string -> t

val to_string : t -> string

(** [distance t1 t2] calculates the "distance" between [t1] and [t2].
    The distance is defined as log2(t1 xor t2), which is equlal to  "# of bits in t1 and t2" - "# of leading 0 bits in (t1 xor t2)".
    For example:
    - distance 0b0000 0b0001 = 1
    - distance 0b0000 0b0010 = 2
    - distance 0b0000 0b0011 = 2
    - distance 0b0000 0b0100 = 3
    - distance 0b0000 0b0111 = 3 *)
val distance : t -> t -> int

val equal : t -> t -> bool

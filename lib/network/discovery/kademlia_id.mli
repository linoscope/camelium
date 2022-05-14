type t

val of_string : string -> t

(** [distance t1 t2] calculates the "distance" between [t1] and [t2].
    The distance is calculated as: "# of bits in t1 and t2" - "# of leading 0 bits in (t1 xor t2)".
    For example:
    - distance 0b0000 0b0001 = 1
    - distance 0b0000 0b0010 = 2
    - distance 0b0000 0b0011 = 2
    - distance 0b0000 0b0100 = 3
    - distance 0b0000 0b0111 = 3

    This definition can be used instead of the "XOR distance" defined in the kademlia paper because
    we only use the "# of leading 0 bits in (t1 xor t2)" in kademlia anyways. *)
val distance : t -> t -> int

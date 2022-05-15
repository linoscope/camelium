open Core

type t = string

let sexp_of_t t =
  t
  |> String.to_list
  |> List.map ~f:(fun c -> sprintf "\\x%02x" (Char.to_int c))
  |> String.concat
  |> Sexp.of_string

let of_string s = s

let to_string t = t

(* calculate "# of bits in t1 and t2" - "# of leading 0 bits in (t1 xor t2)". *)
let distance t1 t2 =
  let count_leading_zero_bits : char list -> int = function
    | [] -> 0
    | byte::_ ->
      let b = Char.to_int byte in
      let leading_zeros = ref 0 in
      let mask = ref 0b10000000 in
      while (b land !mask = 0) do
        Int.incr leading_zeros;
        mask := !mask lsr 1
      done;
      !leading_zeros
  in
  assert (String.length t1 = String.length t2);
  let xor_bytes =
    List.map2_exn
      (String.to_list t1)
      (String.to_list t2)
      ~f:(fun byte1 byte2 -> Int.((Char.to_int byte1) lxor Char.to_int byte2) |> Char.of_int_exn)
  in
  let leading_zeros, non_zeros = xor_bytes |> List.split_while ~f:(fun xor_byte -> Char.(xor_byte = '\x00')) in
  let leading_zero_bits_len = (List.length leading_zeros) * 8 + count_leading_zero_bits non_zeros in
  let bit_len = String.length t1 * 8 in
  bit_len - leading_zero_bits_len

let equal = String.equal

open Base

type item = [
  | `Rlp_data of string
  | `Rlp_list of item list
]
[@@deriving equal]

let encode_int n =
  let rec loop acc n =
    if n = 0 then
      acc |> String.of_char_list
    else
      loop ((Char.of_int_exn (n % 0x100))::acc) (n / 0x100)
  in
  loop [] n

(* TODO: Check if encoded int be larger than 63 bits. *)
let decode_int s =
  s
  |> String.to_list
  |> List.fold ~init:0 ~f:(fun acc c -> acc * 0x100 + Char.to_int c)

let rec encode = function
  | `Rlp_data s ->
    begin match String.length s with
      | 0 -> "\x80"
      | len when len = 1 && Char.(s.[0] < '\x80') -> s
      | len when len < 56 ->
        String.concat [
          encode_int (len + 0x80);
          s
        ]
      | len ->
        let len_str = encode_int len in
        String.concat [
          encode_int (0xb7 + String.length len_str);
          len_str;
          s
        ]
    end
  | `Rlp_list items ->
    match items with
    | [] -> "\xc0"
    | items ->
      let body =
        items
        |> List.map ~f:encode
        |> String.concat
      in
      match String.length body with
      | len when len < 56 ->
        String.concat [
          encode_int (0xc0 + String.length body);
          body
        ]
      | _ ->
        let body_len_str = body |> String.length |> encode_int in
        String.concat [
          encode_int (0xf7 + String.length body_len_str);
          body_len_str;
          body
        ]

let encode_string s = encode (`Rlp_data s)

let decode s =
  (* [loop i] decodes substring [i,) and returns pair (next index to start from, decoded item). *)
  let rec decode i =
    match s.[i] with
    | prefix when Char.(prefix < '\x80') ->
      (i + 1, `Rlp_data s)
    | prefix when Char.(prefix = '\x80') ->
      (i + 1, `Rlp_data "")
    | prefix when Char.(prefix < '\xb8') ->
      let offset = Char.to_int prefix - 0x80 in
      let sub_s = String.sub s ~pos:(i + 1) ~len:offset in
      (i + 1 + offset, `Rlp_data sub_s)
    | prefix when Char.(prefix < '\xc0') ->
      let sub_s_len_len = Char.to_int prefix - 0xb7 in
      let sub_s_len =
        String.sub s ~pos:(i + 1) ~len:sub_s_len_len |> decode_int
      in
      let sub_s = String.sub s ~pos:(i + 1 + sub_s_len_len) ~len:sub_s_len in
      (i + 1 + sub_s_len_len, `Rlp_data sub_s)
    | prefix ->
      (* [decode_list i _end] decodes list of items from substring s[i, _end).
          Returns pair (next index to start from, decoded item) *)
      let decode_list i _end =
        let rec loop acc i =
          if _end <= i then
            (i, List.rev acc)
          else
            let ni, item = decode i in
            loop (item::acc) ni
        in
        loop [] i
      in
      match prefix with
      | _ when Char.(prefix = '\xc0') ->
        (i + 1, `Rlp_list [])
      | _ when Char.(prefix < '\xf8') ->
        let body_len = Char.to_int prefix - 0xc0 in
        let ni, body_list = decode_list (i + 1) (i + body_len) in
        (ni, `Rlp_list body_list)
      | _  ->
        let body_len_len = Char.to_int prefix - 0xf7 in
        let body_len =
          String.sub s ~pos:(i + 1) ~len:body_len_len |> decode_int
        in
        let ni, body_list = decode_list (i + 1 + body_len_len) (i + body_len) in
        (ni, `Rlp_list body_list)
  in
  decode 0 |> snd

open Libsecp256k1.External

let context = Context.create ()

let verify ~pk ~msg ~signature =
  let pk = Key.read_pk_exn context Bigstring.(of_string pk) in
  let msg = Bigstring.of_string msg in
  let signature = Sign.read_exn context Bigstring.(of_string signature) in
  Sign.verify context ~pk ~msg ~signature

open Lwt.Syntax

let with_interval ~sec f =
  let rec loop () =
    let* () = Lwt_unix.sleep sec in
    f ();
    loop ()
  in
  Lwt.async loop

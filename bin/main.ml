open! Lwt
open! Lwt.Syntax


let rec request_response_loop ic oc =
  let* msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some text ->
    let* () = Lwt_io.write_line oc text in
    request_response_loop ic oc
  | None ->
    return ()

let accept_conn conn_fd =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input conn_fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output conn_fd in
  request_response_loop ic oc

let rec server_loop sock =
  let* conn_fd, _ = Lwt_unix.accept sock in
  async (fun () -> accept_conn conn_fd);
  server_loop sock

let () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let sock_addr = ADDR_INET (Unix.inet_addr_loopback, 9000) in
  async (fun () -> bind sock sock_addr);
  listen sock 10;
  Lwt_main.run @@ server_loop sock


(* open! Unix *)
(* open! Sys *)

(* let retransmit fin fout = *)
(*   let buffer_size = 4096 in *)
(*   let buf = Bytes.create buffer_size in *)
(*   let rec copy () = *)
(*     match read fin buf 0 buffer_size with *)
(*     | 0 -> () *)
(*     | n -> ignore (write fout buf 0 n); copy () *)
(*   in *)
(*   copy () *)

(* let client () = *)
(*   if Array.length Sys.argv < 3 then begin *)
(*     prerr_endline "Usage: client <host> <port>"; *)
(*   end; *)
(*   let server_name = Sys.argv.(1) in *)
(*   let port_number = int_of_string Sys.argv.(2) in *)
(*   let server_addr = *)
(*     try *)
(*       (gethostbyname server_name).h_addr_list.(0) *)
(*     with *)
(*     | Not_found -> *)
(*       prerr_endline (server_name ^ ": Host not found"); *)
(*       exit 2 *)
(*   in *)
(*   let sock = socket PF_INET SOCK_STREAM 0 in *)
(*   connect sock (ADDR_INET (server_addr, port_number)); *)
(*   match fork () with *)
(*   | 0 -> *)
(*     retransmit stdin sock; *)
(*     shutdown sock SHUTDOWN_SEND *)
(*   | 1 -> *)
(*     retransmit sock stdout; *)
(*     close stdout; *)
(*     wait (); *)

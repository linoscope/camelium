open Lwt.Syntax

type t = Lwt_unix.file_descr

let create ~host ~port  =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  Lwt.async @@ (fun () -> Lwt_unix.bind socket sockaddr);
  socket

let send t ~msg ~dst =
  Lwt.async (fun () ->
      let msg = Bytes.of_string msg in
      let* _bytes_sent = Lwt_unix.sendto t msg 0 (Bytes.length msg) [] dst in
      Lwt.return_unit)


let listen t f =
  let max_line = 4096 in
  let buf = Bytes.create max_line in
  Lwt.async (fun () ->
      let* (len, addr) = Lwt_unix.recvfrom t buf 0 max_line [] in
      let msg = Bytes.sub buf 0 len |> Bytes.to_string in
      f ~msg ~src:addr;
      Lwt.return_unit)

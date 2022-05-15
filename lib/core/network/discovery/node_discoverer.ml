module Make (Udp_socket : Udp_socket_intf.S) (Scheduler : Scheduler_intf.S) = struct

  type t = {
    udp_socket : Udp_socket.t;
  }

  type action =
    | Send_find_neighbors
    | Recv_find_neighbors
    | Send_ping
    | Recv_ping
    | Send_pong
    | Recv_pong

  let create ~host ~port =
    let udp_socket = Udp_socket.create ~host ~port in
    { udp_socket }

  let run _t = ()

end

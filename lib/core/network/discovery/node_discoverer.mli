module Make (Udp_socket : Udp_socket_intf.S) (Scheduler : Scheduler_intf.S) : sig
  type t
  val create : host:string -> port:int -> t
  val run : t -> unit
end

module type S = sig
  type t
  val create : host:string -> port:int -> t
  val send : t -> msg:string -> dst:Unix.sockaddr -> unit
  val listen : t -> (msg:string -> src:Unix.sockaddr -> unit) -> unit
end

module type S = sig
  val with_interval : sec:float -> (unit -> unit) -> unit
end

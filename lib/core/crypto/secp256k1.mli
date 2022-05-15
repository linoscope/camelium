(** Utility functions for secp256k1 *)

val verify : pk:string -> msg:string -> signature:string -> (bool, string) Result.result

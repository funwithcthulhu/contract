(** Pure HTTP-like response value used by the response validator. *)

type t = { status : int; body : Yojson.Safe.t option }

val make : ?body:Yojson.Safe.t -> status:int -> unit -> t
val status : t -> int
val body : t -> Yojson.Safe.t option

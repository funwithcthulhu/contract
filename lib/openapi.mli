(** OpenAPI document input for a list of endpoint contracts. *)

type api = { title : string; version : string; endpoints : Endpoint.t list }

val to_yojson : api -> Yojson.Safe.t
(** Emit OpenAPI 3.0.3 JSON for the supplied endpoint list. *)

val to_string : ?pretty:bool -> api -> string

(** Structured validation and decoding errors. *)

type location =
  | Method
  | Route
  | Path_param of string
  | Query_param of string
  | Body
  | Json_field of string

type t = {
  location : location;
  message : string;
  expected : string option;
  got : string option;
}

(** [make ~location message] constructs an error with optional expected and
    observed values for diagnostics. *)
val make :
  ?expected:string ->
  ?got:string ->
  location:location ->
  string ->
  t

val to_string : t -> string

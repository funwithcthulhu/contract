(** Structured validation and decoding errors. *)

type location =
  | Method
  | Status
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

val make : ?expected:string -> ?got:string -> location:location -> string -> t
(** [make ~location message] constructs an error with optional expected and
    observed values for diagnostics. *)

val to_string : t -> string

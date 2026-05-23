type 'a t = {
  name : string;
  schema : Schema.t;
  encode : 'a -> string;
  decode : string -> ('a, Error.t) result;
}

val string : string t
val int : int t
val bool : bool t
val float : float t

type api = {
  title : string;
  version : string;
  endpoints : Endpoint.t list;
}

val to_yojson : api -> Yojson.Safe.t
val to_string : ?pretty:bool -> api -> string

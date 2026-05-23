type t = {
  method_ : Endpoint.method_;
  path : string;
  query : (string * string) list;
  body : Yojson.Safe.t option;
}

let make ?(query = []) ?body ~method_ ~path () =
  { method_; path; query; body }

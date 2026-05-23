type t = {
  meth : Endpoint.meth;
  path : string;
  query : (string * string) list;
  body : Yojson.Safe.t option;
}

let make ?(query = []) ?body ~meth ~path () =
  { meth; path; query; body }

type t = {
  status : int;
  body : Yojson.Safe.t option;
}

let make ?body ~status () = { status; body }

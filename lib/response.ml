type t = { status : int; body : Yojson.Safe.t option }

let make ?body ~status () = { status; body }
let status response = response.status
let body response = response.body

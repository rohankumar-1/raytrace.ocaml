
open Raytrace.Tuple


let p1 = point (0., 0., 1.)
let p2 = vector (1., 1., 1.)

let () = p1#is_point |> string_of_bool |> print_endline
let () = p2#is_point |> string_of_bool |> print_endline
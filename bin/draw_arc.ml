
open Data
(* open Util *)
open Draw

(*
This file defines a projectile in terms of tuples of position, and then uses this projectile and its movement to write an arc 
to a canvas .ppm (output file)
*)

type projectile = {
  pos : tuple;
  vel : tuple;
}

let tick env proj = {
  pos = tadd proj.pos proj.vel;
  vel = tadd (tadd proj.vel (fst env)) (snd env)
}


let draw_arc can env p = 
  let rec aux a =
    let (pix_x, pix_y) = (int_of_float a.pos.x, ((can.height - int_of_float a.pos.y) - 1)) in
    (* Printf.fprintf stdout "%b %b\n" (pix_x < 0) (pix_y < 0); *)
    if (pix_y < 0 || pix_y >= can.height || pix_x < 0 || pix_x >= can.width) then begin
      ()
    end
    else begin
      can.grid.(pix_x).(pix_y) <- make_color 1. 1. 0. ;
      aux (tick env a);
      ()
    end
  in aux p
;;


let () =
  let start = {
    pos = point 0. 1. 0. ;
    vel = tmult (norm (vector 1. 1.8 0.)) 11.25
  } in
  let environment = (
    vector 0. (-0.1) 0.,
    vector (-0.01) 0. 0. 
  ) in 
  let c = make_canvas ~w:900 ~h:550 in
  let oc = open_out "test.ppm" in
    draw_arc c environment start;
    write_canvas_P6 ~oc:oc ~can:c;

    

open Raytrace.Tuple
(* open Raytrace.Util *)
open Raytrace.Draw

(*
This file defines a projectile in terms of tuples of position, and then uses this projectile and its movement to write an arc 
to a canvas .ppm (output file)
*)

type projectile = {
  pos : tuple;
  vel : tuple;
}

let tick env proj = {
  pos = proj.pos ++ proj.vel;
  vel = proj.vel ++ (fst env) ++ (snd env)
}


let draw_arc can env p = 
  let rec aux a =
    let (pix_x, pix_y) = (int_of_float a.pos.x, ((can.height - int_of_float a.pos.y) - 1)) in
    (* Printf.fprintf stdout "%b %b\n" (pix_x < 0) (pix_y < 0); *)
    if (pix_y < 0 || pix_y >= can.height || pix_x < 0 || pix_x >= can.width) then begin
      ()
    end
    else begin
      can.grid.(pix_y).(pix_x) <- Color (build_color 1. 1. 0.) ;
      let newP = tick env a in
      Printf.fprintf stdout "%d %d\n" (int_of_float a.pos.x) (can.height - int_of_float a.pos.y);
      (* Printf.fprintf stdout "%f %f\n" (a.pos.x) (a.pos.y); *)
      aux newP;
      ()
    end
  in aux p
;;


let () =
  let start = {
    pos = point 0. 1. 0. ;
    vel = (norm (vector 1. 1.8 0.)) *** 11.25
  } in
  let environment = (
    vector 0. (-0.1) 0.,
    vector (-0.01) 0. 0. 
  ) in 
  let c = make_canvas ~h:900 ~w:550 in
  let oc = open_out "test.ppm" in
    draw_arc c environment start;
    write_canvas ~oc:oc ~can:c;

    
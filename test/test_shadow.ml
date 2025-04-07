open OUnit2
open Graphics
open Draw
open Data
(* open Transform
open Util *)


let m = make_material ();;
let pos = point 0. 0. 0.;;

let test_lighting_shadow _ = 
  let eyev = vector 0. 0. (-1.) in 
  let normv = vector 0. 0. (-1.) in 
  let light = make_light (make_color 1. 1. 1.) (point 0. 0. (-10.)) in 
  let res = lighting m light pos eyev normv true in 

  assert_bool "shadow" (cequal (pix_to_col res) (make_color 0.1 0.1 0.1))


let suite = 
  "Shadow tests" >::: [
    "Test lighting shadow" >:: test_lighting_shadow;
  ]

let () = run_test_tt_main suite
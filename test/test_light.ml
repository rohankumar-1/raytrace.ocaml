open OUnit2
open Graphics
open Data
open Transform
open Draw
open Shapes

let test_normal_at _ = 
  let sph = new sphere in 
  let n = sph#normal_at (point 0. 0. 1.) in
  assert_bool "test norm_at" (tequal n (vector 0. 0. 1.));
  assert_bool "test norm_at 2" (tequal n (norm n))

let test_norm_at_2 _ =
  let sph = new sphere in 
  sph#set_transform (translate 0. 1. 0.);
  let n = sph#normal_at (point 0. 1.70711 (-0.70711)) in
  assert_bool "test norm_at" (tequal n (vector 0. 0.70711 (-0.70711)))

let test_reflect _ =
  let v = vector 0. (-1.) 0. in 
  let temp = (sqrt 2.) /. 2. in
  let n = vector temp temp 0. in
  let r = reflect v n in
  assert_bool "test norm_at" (tequal r (vector 1. 0. 0.))


let m = make_material ();;
let pos = point 0. 0. 0.;;

let test_light_1 _ = 
  let eyev = vector 0. 0. (-1.) in 
  let normv = vector 0. 0. (-1.) in 
  let light = make_light (make_color 1. 1. 1.) (point 0. 0. (10.)) in 
  let res = lighting m light pos eyev normv false in 
  assert_bool "check light (1)" (cequal (pix_to_col res) (make_color 0.1 0.1 0.1))

let suite = 
  "Light tests" >::: [
    "Test norm_at (1)" >:: test_normal_at;
    "Test norm_at (2)" >:: test_norm_at_2;
    "Test reflect" >:: test_reflect;
    "Test lighting (1)" >:: test_light_1;
  ]

let () = run_test_tt_main suite
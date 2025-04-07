open OUnit2
open Graphics
open Data
open Transform
open Draw
open Shapes



let test_pattern_1 _ = 
  let obj = new sphere in 
  obj#set_transform (scale 2. 2. 2.);

  let pat = make_stripe (make_color 1. 1. 1.) (make_color 0. 0. 0.) (scale 2. 2. 2.) in 
  let c = get_pattern (obj#get_transform) (point 2.5 0. 0.) pat in 
  assert_bool "" (cequal c (make_color 1. 1. 1.))


let test_pattern_2 _ = 
  let obj = new sphere in 
  let pat = make_stripe (make_color 1. 1. 1.) (make_color 0. 0. 0.) (scale 2. 2. 2.) in 
  let c = get_pattern (obj#get_transform) (point 1.5 0. 0.) pat in 
  assert_bool "" (cequal c (make_color 1. 1. 1.))
  
    

let suite = 
  "Pattern tests" >::: [
    "Test pattern (1)" >:: test_pattern_1;
    "Test pattern (2)" >:: test_pattern_2;
  ]

let () = run_test_tt_main suite
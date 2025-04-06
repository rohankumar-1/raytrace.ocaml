open Raytrace.Draw
open OUnit2

let test_create_color _ =
  let col = {red=0.5; green=0.4;blue=0.7} in
  assert_equal col.red 0.5;
  assert_equal col.green 0.4;
  assert_equal col.blue 0.7;
;;

let test_add_color _ =
  let c1 = {red=0.5; green=0.4;blue=0.7} in
  let c2 = {red=0.2; green=(-0.4);blue=0.7} in
  let s = add_clr c1 c2 in
  assert_equal s.red 0.7;
  assert_equal s.green 0.0;
  assert_equal s.blue 1.4;
;;

let test_sub_color _ =
  let c1 = {red=0.5; green=0.4;blue=0.7} in
  let c2 = {red=0.2; green=(-0.4);blue=0.7} in
  let s = sub_clr c1 c2 in
  assert_equal s.red 0.3;
  assert_equal s.green 0.8;
  assert_equal s.blue 0.0;
;;

let test_scalar_mult_color _ =
  let c1 = {red=0.5; green=0.4;blue=0.7} in
  let s = mult_clr c1 2. in
  assert_equal s.red 1.0;
  assert_equal s.green 0.8;
  assert_equal s.blue 1.4;
;;

let test_hadamard _ =
  let c1 = {red=0.5; green=0.4;blue=0.7} in
  let c2 = {red=0.2; green=(-0.4);blue=0.7} in
  let s = hadamard c1 c2 in
  let g_truth = {red=0.1; green=(-0.16); blue=0.49} in
  assert_bool "checking using float error" (cequal s g_truth);
;;


let suite = 
  "Color tests" >::: [
    "Test creation" >:: test_create_color;
    "Test add" >:: test_add_color;
    "Test sub" >:: test_sub_color;
    "Test scalar mult" >:: test_scalar_mult_color;
    "Test hadamard product" >:: test_hadamard;
  ]

let () = run_test_tt_main suite
open OUnit2
open Graphics
open Draw
open Data
open Transform
open Shapes


let test_plane_1 _ = 
  let p = new plane in 
  let r = Ray.make ~p:(point 0. 1. 0.) ~v:(vector 0. (-1.) 0.) in 
  let xs = p#local_intersect r in
  assert_equal 1 (List.length xs);
  assert_bool "" (equal_float (List.nth xs 0) 1.) 


let suite =
  "Test Plane" >::: [
    "test_plane_1" >:: test_plane_1;
  ]

let () =
  run_test_tt_main suite
open Raytrace.Tuple
open OUnit2

let test_tuple_pnt _ = 
  let t = point 4.3 (-4.2) 3.1 in
  assert_equal 4.3 t.x;
  assert_equal (-4.2) t.y;
  assert_equal 3.1 t.z;
  assert_equal 1.0 t.w;
  assert_equal true (is_point t);
  assert_equal false (is_vector t);
;;

let test_tuple_vec _ = 
  let t = vector 4.3 (-4.2) 3.1 in
  assert_equal 4.3 t.x;
  assert_equal (-4.2) t.y;
  assert_equal 3.1 t.z;
  assert_equal 0.0 t.w;
  assert_equal false (is_point t);
  assert_equal true (is_vector t);
;;

let test_pnt _ = 
    let p = point 4. (-4.) 3. in
    let t = {x=4.; y=(-4.); z=3.; w=1.} in
    assert_equal p.w t.w;
    assert_equal p.x t.x;
    assert_equal p.y t.y;
    assert_equal p.z t.z
;;

let test_vec _ = 
  let p = vector 4. (-4.) 3. in
  let t = {x=4.; y=(-4.); z=3.; w=0.} in
  assert_equal p.w t.w;
  assert_equal p.x t.x;
  assert_equal p.y t.y;
  assert_equal p.z t.z
;;

let test_add _ = 
  let t1 = {x=3.; y=(-2.); z=5.; w=1.} in
  let t2 = {x=(-2.); y=3.; z=1.; w=0.} in
  let s = add_tup t1 t2 in
  assert_equal s.w 1.0;
  assert_equal s.x 1.0;
  assert_equal s.y 1.0;
  assert_equal s.z 6.0
;;

let test_sub_1 _ = 
  let t1 = point 3. 2. 1. in
  let t2 = point 5. 6. 7. in
  let s = sub_tup t1 t2 in
  assert_equal s.w 0.0;
  assert_equal s.x (-2.0);
  assert_equal s.y (-4.0);
  assert_equal s.z (-6.0)
;;

let test_sub_2 _ = 
  let t1 = point 3. 2. 1. in
  let t2 = vector 5. 6. 7. in
  let s = sub_tup t1 t2 in
  assert_equal s.w 1.0;
  assert_equal s.x (-2.0);
  assert_equal s.y (-4.0);
  assert_equal s.z (-6.0)
;;

let test_sub_3 _ = 
  let t1 = vector 3. 2. 1. in
  let t2 = vector 5. 6. 7. in
  let s = sub_tup t1 t2 in
  assert_equal s.w 0.0;
  assert_equal s.x (-2.0);
  assert_equal s.y (-4.0);
  assert_equal s.z (-6.0)
;;

let test_negate _ = 
  let t1 = {x=1.; y=(-2.); z=3.; w=(-4.)} in
  let s = ~~ t1 in
  assert_equal s.w (4.0);
  assert_equal s.x (-1.0);
  assert_equal s.y (2.0);
  assert_equal s.z (-3.0)
;;

let test_scalar_mult _ = 
  let t1 = {x=1.; y=(-2.); z=3.; w=(-4.)} in
  let s = t1 *** 3.5 in
  assert_equal s.w (-14.0);
  assert_equal s.x (3.5);
  assert_equal s.y (-7.0);
  assert_equal s.z (10.5)
;;

let test_scalar_div _ =
  let t1 = {x=1.; y=(-2.); z=3.; w=(-4.)} in
  let s = t1 // 2. in
  assert_equal s.w (-2.0);
  assert_equal s.x (0.5);
  assert_equal s.y (-1.0);
  assert_equal s.z (1.5)
;;

let test_magnitude_1 _ =
  let t1 = vector 1. 2. 3. in
  assert_equal (sqrt 14.) (magnitude t1) 
;;

let test_magnitude_2 _ =
  let t1 = vector 0. 0. (1.) in
  assert_equal (1.) (magnitude t1) 
;;

let test_magnitude_3 _ =
  let t1 = vector (-1.) (-2.) (-3.) in
  assert_equal (sqrt 14.) (magnitude t1) 
;;

let test_norm _ =
  let t1 = vector (-4.) 0. 0. in
  let s = norm t1 in
  assert_equal s.x (-1.0);
  assert_equal s.y (0.0);
  assert_equal s.z (0.0);
  assert_equal s.w (0.0);
;;

let test_dot _ = 
  let t1 = vector 1. 2. 3. in
  let t2 = vector 2. 3. 4. in
  assert_equal (dot t1 t2) 20. ;
;;

let test_cross _ =
  let t1 = vector 1. 2. 3. in
  let t2 = vector 2. 3. 4. in
  let s1 = t1 *@ t2 in
  let s2 = t2 *@ t1 in
  assert_equal s1.x (-1.0);
  assert_equal s1.y (2.0);
  assert_equal s1.z (-1.0);
  assert_equal s2.x (1.0);
  assert_equal s2.y (-2.0);
  assert_equal s2.z (1.0);
;;

let suite = 
    "Tuple tests" >::: [
      "Test tuple creation (vector)" >:: test_tuple_vec;
      "Test tuple creation (point)" >:: test_tuple_pnt;
      "Test vector factory" >:: test_vec;
      "Test point factory" >:: test_pnt;
      "Test add_tuple" >:: test_add;
      "Test sub_tuple (1)" >:: test_sub_1;
      "Test sub_tuple (2)" >:: test_sub_2;
      "Test sub_tuple (3)" >:: test_sub_3;
      "Test negate" >:: test_negate;
      "Test scalar mult" >:: test_scalar_mult;
      "Test scalar div" >:: test_scalar_div;
      "Test magnitude (1)" >:: test_magnitude_1;
      "Test magnitude (2)" >:: test_magnitude_2;
      "Test magnitude (3)" >:: test_magnitude_3;
      "Test norm" >:: test_norm;
      "Test dot" >:: test_dot;
      "Test cross" >:: test_cross;
    ]

let () = run_test_tt_main suite
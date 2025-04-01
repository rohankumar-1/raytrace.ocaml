open Raytrace.Tuple
open OUnit2

let test_tuple_pnt _ = 
  let t = new tuple (4.3, -4.2, 3.1, 1.0) in
  assert_equal 4.3 t#gx;
  assert_equal (-4.2) t#gy;
  assert_equal 3.1 t#gz;
  assert_equal 1.0 t#gw;
  assert_equal true t#is_point;
  assert_equal false t#is_vector
;;

let test_tuple_vec _ = 
  let t = new tuple (4.3, -4.2, 3.1, 0.0) in
  assert_equal 4.3 t#gx;
  assert_equal (-4.2) t#gy;
  assert_equal 3.1 t#gz;
  assert_equal 0.0 t#gw;
  assert_equal false t#is_point;
  assert_equal true t#is_vector
;;

let test_pnt _ = 
    let p = point (4., -4., 3.) in
    let t = new tuple (4., -4., 3., 1.) in
    assert_equal p#gw t#gw;
    assert_equal p#gx t#gx;
    assert_equal p#gy t#gy;
    assert_equal p#gz t#gz
;;

let test_vec _ = 
  let p = vector (4., -4., 3.) in
  let t = new tuple (4., -4., 3., 0.) in
  assert_equal p#gw t#gw;
  assert_equal p#gx t#gx;
  assert_equal p#gy t#gy;
  assert_equal p#gz t#gz
;;

let test_add _ = 
  let t1 = new tuple (3., -2., 5., 1.) in
  let t2 = new tuple (-2., 3., 1., 0.) in
  let s = add_tup t1 t2 in
  assert_equal s#gw 1.0;
  assert_equal s#gx 1.0;
  assert_equal s#gy 1.0;
  assert_equal s#gz 6.0
;;

let test_sub_1 _ = 
  let t1 = point (3., 2., 1.) in
  let t2 = point (5., 6., 7.) in
  let s = sub_tup t1 t2 in
  assert_equal s#gw 0.0;
  assert_equal s#gx (-2.0);
  assert_equal s#gy (-4.0);
  assert_equal s#gz (-6.0)
;;

let test_sub_2 _ = 
  let t1 = point (3., 2., 1.) in
  let t2 = vector (5., 6., 7.) in
  let s = sub_tup t1 t2 in
  assert_equal s#gw 1.0;
  assert_equal s#gx (-2.0);
  assert_equal s#gy (-4.0);
  assert_equal s#gz (-6.0)
;;

let test_sub_3 _ = 
  let t1 = vector (3., 2., 1.) in
  let t2 = vector (5., 6., 7.) in
  let s = sub_tup t1 t2 in
  assert_equal s#gw 0.0;
  assert_equal s#gx (-2.0);
  assert_equal s#gy (-4.0);
  assert_equal s#gz (-6.0)
;;

let test_negate _ = 
  let t1 = new tuple (1., -2., 3., -4.) in
  let s = ~~ t1 in
  assert_equal s#gw (4.0);
  assert_equal s#gx (-1.0);
  assert_equal s#gy (2.0);
  assert_equal s#gz (-3.0)
;;

let test_scalar_mult _ = 
  let t1 = new tuple (1., -2., 3., -4.) in
  let s = t1 *** 3.5 in
  assert_equal s#gw (-14.0);
  assert_equal s#gx (3.5);
  assert_equal s#gy (-7.0);
  assert_equal s#gz (10.5)
;;

let test_scalar_div _ =
  let t1 = new tuple (1., -2., 3., -4.) in
  let s = t1 // 2. in
  assert_equal s#gw (-2.0);
  assert_equal s#gx (0.5);
  assert_equal s#gy (-1.0);
  assert_equal s#gz (1.5)
;;

let test_magnitude_1 _ =
  let t1 = vector (1., 2., 3.) in
  assert_equal (sqrt 14.) (magnitude t1) 
;;

let test_magnitude_2 _ =
  let t1 = vector (0., 0., 1.) in
  assert_equal (1.) (magnitude t1) 
;;

let test_magnitude_3 _ =
  let t1 = vector (-1., -2., -3.) in
  assert_equal (sqrt 14.) (magnitude t1) 
;;

let test_norm _ =
  let t1 = vector (-4., 0., 0.) in
  let s = norm t1 in
  assert_equal s#gx (-1.0);
  assert_equal s#gy (0.0);
  assert_equal s#gz (0.0);
  assert_equal s#gw (0.0);
;;

let test_dot _ = 
  let t1 = vector (1., 2., 3.) in
  let t2 = vector (2., 3., 4.) in
  assert_equal (dot t1 t2) 20. ;
;;

let test_cross _ =
  let t1 = vector (1., 2., 3.) in
  let t2 = vector (2., 3., 4.) in
  let s1 = t1 *@ t2 in
  let s2 = t2 *@ t1 in
  assert_equal s1#gx (-1.0);
  assert_equal s1#gy (2.0);
  assert_equal s1#gz (-1.0);
  assert_equal s2#gx (1.0);
  assert_equal s2#gy (-2.0);
  assert_equal s2#gz (1.0);
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
open OUnit2
open Graphics
open Draw
open Data
open Transform
open Shapes

let w = default_world ();;
let test_intersect _ = 
  let r =  Ray.make ~p:(point 0. 0. (-5.)) ~v:(vector 0. 0. 1.) in 

  let xs = intersect_world w r in
  assert_equal (List.length xs) 4;
  assert_bool "" (equal_float (List.nth xs 0).t 4.0);
  assert_bool "" (equal_float (List.nth xs 1).t 4.5);
  assert_bool "" (equal_float (List.nth xs 2).t 5.5);
  assert_bool "" (equal_float (List.nth xs 3).t 6.0);
;;


let test_prep_comp _ = 
  let r = Ray.make ~p:(point 0. 0. (-5.)) ~v:(vector 0. 0. 1.) in 
  let sph = ref new sphere in 
  let i = {obj=(Some sph); t=4.} in
  let comp = prepare_computations i r in 
  assert_bool "" (not comp.inside);
  assert_bool "pt" (tequal (comp.pt) (point 0. 0. (-1.)));
  assert_bool "ev" (tequal (comp.eyev) (vector 0. 0. (-1.)));
  assert_bool "nv" (tequal (comp.normv) (vector 0. 0. (-1.)))


let test_prep_comp_2 _ = 
  let r = Ray.make ~p:(point 0. 0. 0.) ~v:(vector 0. 0. 1.) in 
  let sph = ref new sphere in 
  let i = {obj=(Some sph); t=1.} in
  let comp = prepare_computations i r in 
  assert_bool "" comp.inside;
  assert_bool "" (tequal (comp.pt) (point 0. 0. 1.));
  assert_bool "" (tequal (comp.eyev) (vector 0. 0. (-1.)));
  assert_bool "" (tequal (comp.normv) (vector 0. 0. (-1.)))



let test_cam _ = 
  let c = make_camera 200 125 (Float.pi /. 2.) in 
  assert_bool "" (equal_float c.pixel_size 0.01)


let test_rfc _ = 
  let c = make_camera 201 101 (Float.pi /. 2.) in 
  let r = ray_for_pixel c 100. 50. in 

  assert_bool "" (tequal (Ray.get_origin r) (point 0. 0. 0.));
  assert_bool "" (tequal (Ray.get_dir r) (vector 0. 0. (-1.)))


let test_rfc_2 _ = 
  let c = make_camera 201 101 (Float.pi /. 2.) in 
  c.tf := (chain_transforms [
    rotate_y (Float.pi /. 4.);
    translate 0. (-2.) 5.; 
  ]);
  let r = ray_for_pixel c 100. 50. in 
  let sqrt2_2 = (sqrt 2. /. 2.) in 
  assert_bool "this" (tequal (Ray.get_origin r) (point 0. 2. (-5.)));
  assert_bool "that" (tequal (Ray.get_dir r) (vector sqrt2_2 0. (-.sqrt2_2)))


let test_view_transform _ = 
  let fromv = point 1. 3. 2. in 
  let tov = point 4. (-2.) 8. in 
  let upv = vector 1. 1. 0. in 

  let t = view_transform fromv tov upv in 
  assert_bool "" 
  (mequal t
    ([|
      [|-0.50709 ; 0.50709 ;  0.67612 ;  -2.36643 |];
      [|0.76772 ; 0.60609 ; 0.12122 ; -2.82843|];
      [|-0.35857 ; 0.59761 ; -0.71714 ; 0.00000|];
      [| 0.00000 ; 0.00000 ; 0.00000 ; 1.00000  |];
    |]))


let test_color_at _ = 
  let r = Ray.make ~p:(point 0. 0. (-5.)) ~v:(vector 0. 1. 0.) in 
  let c = color_at w r in 
  assert_bool "" (cequal  c (make_color 0. 0. 0.))


let test_color_at_2 _ = 
  let r = Ray.make ~p:(point 0. 0. (-5.)) ~v:(vector 0. 0. 1.) in 
  let c = color_at w r in 
  assert_bool "" (cequal c (make_color 0.38066 0.47583 0.2855))

let test_render _ = 
  let c = make_camera 11 11 (Float.pi /. 2.) in 
  let fromv = point 0. 0. (-5.) in 
  let tov = point 0. 0. 0. in 
  let upv = vector 0. 1. 0. in 
  
  c.tf := view_transform fromv tov upv;
  let img = render c w in 
  assert_bool "render" (cequal img.grid.(5).(5) (make_color 0.38066 0.47583 0.2855))


let suite = 
  "World tests" >::: [
    "Test intersect world" >:: test_intersect;
    "Test computation prep" >:: test_prep_comp;
    "Test computation prep (2)" >:: test_prep_comp_2;
    "Test camera" >:: test_cam;
    "Test color" >:: test_color_at;
    "Test color (2)" >:: test_color_at_2;
    "Test rfc" >:: test_rfc;
    "Test rfc (2)" >:: test_rfc_2;
    "Test view transform" >:: test_view_transform;
    "Test render" >:: test_render;
  ]

let () = run_test_tt_main suite
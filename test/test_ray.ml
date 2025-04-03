open Raytrace.Data
open Raytrace.Graphics
open OUnit2

let test_ray_create _ = 
  let r = Ray.make_ray ~p:(point 2. 3. 4.) ~v:(vector 1. 0. 0.) in
  assert_bool "test creation + position" (tequal (Ray.position r 0.) (point 2. 3. 4.));
  assert_bool "test creation + position" (tequal (Ray.position r 1.) (point 3. 3. 4.));
  assert_bool "test creation + position" (tequal (Ray.position r 2.5) (point 4.5 3. 4.))

let test_ray_intersect _ = 
  let s = new sphere in
  let r = Ray.make_ray ~p:(point 0. 0. 5.) ~v:(vector 0. 0. 1.) in
  let res = Ray.intersect r s in
  assert_equal (List.nth res 0).obj (Oo.id s)

let test_intersection_obj _ = 
  let s = new sphere in
  let i = {obj=(Oo.id s); t=3.5} in
  assert_equal i.t 3.5;
  assert_equal i.obj (Oo.id s)


let test_hit _ = 
  let s = new sphere in 
  let i1 = {obj=(Oo.id s); t=1.} in
  let i2 = {obj=(Oo.id s); t=2.} in
  let i3 = {obj=(Oo.id s); t=(-1.)} in
  let res = hit [i1;i2;i3] in 
  assert_equal i1.t res.t

let suite = 
  "Ray tests" >::: [
    "Test ray creation + position" >:: test_ray_create;
    "Test ray intersect" >:: test_ray_intersect;
    "Test intersect" >:: test_intersection_obj;
    "Test hit" >:: test_hit;
  ]


let () = run_test_tt_main suite
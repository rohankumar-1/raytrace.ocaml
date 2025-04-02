open Raytrace.Transform
open Raytrace.Data
open OUnit2


let test_translate _ = 
  let tr = (translate 5. (-3.) 2.) in
  let p = (point (-3.) 4. 5.) in
  let check = point 2. 1. 7. in
  (* print_endline @@ mat_to_string @@ tr;
  print_endline @@ mat_to_string @@ m_from_tuple @@ check;
  print_endline @@ mat_to_string @@ (matmul tr (m_from_tuple p)); *)
  assert_bool "translate test failed"(mequal (matmul tr (m_from_tuple p)) (m_from_tuple check))

let test_translate_vec _ = 
  let tr = (translate 5. (-3.) 2.) in
  let v = m_from_tuple (vector (-3.) 4. 5.) in
  (* print_endline @@ mat_to_string @@ tr;
  print_endline @@ mat_to_string @@ m_from_tuple @@ check;
  print_endline @@ mat_to_string @@ (matmul tr (m_from_tuple p)); *)
  assert_bool "translate test failed"(mequal (matmul tr v) v)


let test_transform _ = 
  let tr = (scale (-1.) 1. 1.) in
  let p = (point 2. 3. 4.) in
  let check =  (point (-2.) 3. 4.) in
  assert_bool "checking transform" (mequal (matmul tr (m_from_tuple p)) (m_from_tuple check))


let test_chain _ = 
  let a = rotate_x (Float.pi /. 2.) in
  let b = scale 5. 5. 5. in
  let c = translate 10. 5. 7. in 
  let chain = matmul c (matmul b a) in
  assert_bool "testing chain op" (mequal (matmul chain (m_from_list [[1.];[0.];[1.];[1.]])) (m_from_list [[15.];[0.];[7.];[1.]]))


let test_chain_func _ = 
  let a = rotate_x (Float.pi /. 2.) in
  let b = scale 5. 5. 5. in
  let c = translate 10. 5. 7. in 
  let ch = (chain_transforms [a;b;c]) in
  assert_bool "testing chain op" (mequal (matmul ch (m_from_list [[1.];[0.];[1.];[1.]])) (m_from_list [[15.];[0.];[7.];[1.]]))
  
let suite = 
  "Transform tests" >::: [
    "Test translate on point" >:: test_translate;
    "Test translate on vector" >:: test_translate_vec;
    "Test scale" >:: test_transform;
    "Test chain op" >:: test_chain;
    "Test chain func " >:: test_chain_func;
  ]


let () = run_test_tt_main suite

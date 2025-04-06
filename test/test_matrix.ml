open Raytrace.Data
open OUnit2

let test_matrix_from_list _ = 
  let lst = [[1.; 2.; 3.];[4.; 5.; 6.];[7.; 8.; 9.;]] in
  let m = m_from_list lst in
  assert_equal m.(0).(0) 1.;
  assert_equal m.(0).(1) 2.;
  assert_equal m.(2).(2) 9.;
;;

let test_matrix_from_tuple _ = 
  let tuple = vector 1. 2. 3. in
  let m = m_from_tuple tuple in
  assert_equal m.(0).(0) 1.;
  assert_equal m.(1).(0) 2.;
  assert_equal m.(2).(0) 3.;
  assert_equal m.(3).(0) 0.;
;;

let test_id_matrix _ = 
  let m = identity 3 in
  assert_equal m.(0).(0) 1.;
  assert_equal m.(1).(0) 0.;
  assert_equal m.(2).(2) 1.;
;;

let test_matmul _ =
  let a = m_from_list [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;8.;7.;6.];[5.;4.;3.;2.]] in
  let b = m_from_list [[-2.;1.;2.;3.];[3.;2.;1.;-1.];[4.;3.;6.;5.];[1.;2.;7.;8.;]] in
  let check = m_from_list [[20.;22.;50.;48.];[44.;54.;114.;108.];[40.;58.;110.;102.];[16.;26.;46.;42.;]] in
  let s = matmul a b in
  assert_bool "matmul op failed" (mequal s check)

let test_matmul_vec _ =
  let a = m_from_list [[1.;2.;3.;4.];[2.;4.;4.;2.];[8.;6.;4.;1.];[0.;0.;0.;1.]] in
  let t = m_from_tuple (point 1. 2. 3.) in
  let check = m_from_list [[18.];[24.];[33.];[1.;]] in
  let s = matmul a t in
  assert_bool "matmul op failed" (mequal s check)
  

let test_transpose _ = 
  let a = m_from_list [[0.;9.;3.;0.];[9.;8.;0.;8.;];[1.;8.;5.;3.;];[0.;0.;5.;8.;]] in
  let check = m_from_list [[0.;9.;1.;0.];[9.;8.;8.;0.;];[3.;0.;5.;5.;];[0.;8.;3.;8.;]] in
  assert_bool "transpose faied" (mequal (transpose a) check)



let test_submat_1 _ = 
  let a = m_from_list [[1.;5.;0.];[-3.;2.;7.];[0.;6.;-3.]] in
  let check = m_from_list [[-3.;2.];[0.;6.]] in
  assert_bool "transpose faied" (mequal (submatrix 0 2 a) check)
  
let test_determinant_1 _ = 
  let a = m_from_list [[1.;2.;6.;];[-5.;8.;-4.];[2.;6.;4.]] in
  assert_bool "failed determinant test" (equal_float (determinant a) (-196.))


let test_determinant_2 _ = 
  let a = m_from_list [[-2.;-8.;3.;5.];[-3.;1.;7.;3.];[1.;2.;-9.;6.];[-6.;7.;7.;-9.]] in
  assert_bool "failed determinant test" (equal_float (determinant a) (-4071.))


let test_inverse_1 _ = 
  let a = m_from_list [[-5.;2.;6.;-8.];[1.;-5.;1.;8.];[7.;7.;-6.;-7.];[1.;-3.;7.;4.]] in
  let check = m_from_list [[0.21805;0.45113;0.24060;-0.04511];
                            [-0.80827;-1.45677;-0.44361;0.52068];
                            [-0.07895;-0.22368;-0.05263;0.19737];
                            [-0.52256;-0.81391;-0.30075;0.30639]] in
  (* print_newline (); print_endline (mat_to_string (a));
  print_newline (); print_float (cofactor 2 3 a); print_newline ();
  print_newline (); print_endline (mat_to_string (invert a)); *)
  assert_bool "failed inverse test" (mequal (invert a) (check))

let test_inverse_2 _ = 
  let a = m_from_list [[8.;-5.;9.;2.];[7.;5.;6.;1.];[-6.;0.;9.;6.];[-3.;0.;-9.;-4.]] in
  let check = m_from_list [[-0.15385;-0.15385;-0.28205; -0.53846];
                           [-0.07692;0.12308;0.02564;0.03077];
                           [0.35897;0.35897;0.43590;0.92308];
                           [-0.69231;-0.69231;-0.76923;-1.92308]] in
  (* print_newline (); print_endline (mat_to_string (a));
  print_newline (); print_endline (mat_to_string (invert a)); *)
  assert_bool "failed inverse test" (mequal (invert a) (check))

let suite = 
  "Matrix tests" >::: [
    "Test matrix creation from list" >:: test_matrix_from_list;
    "Test matrix creation from tuple" >:: test_matrix_from_tuple;
    "Test ID matrix" >:: test_id_matrix;
    "Test matmul" >:: test_matmul;
    "Test matmul vec" >:: test_matmul_vec;
    "Test transpose" >:: test_transpose;
    "Test submat (1)" >:: test_submat_1;
    "Test determinant (3x3)" >:: test_determinant_1;
    "Test determinant (4x4)" >:: test_determinant_2;
    "Test inverse 1 (4x4)" >:: test_inverse_1;
    "Test inverse 2 (4x4)" >:: test_inverse_2;
  ]

let () = run_test_tt_main suite
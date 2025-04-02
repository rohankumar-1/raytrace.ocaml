open Util

type tuple = {x:float ; y:float ; z:float ; w:float}
let point x y z = {x=x ; y=y ; z=z ; w=1.0} 
let vector x y z = {x=x; y=y ; z=z ; w=0.0} 
let t_from_mat a = {x=a.(0).(0); y=a.(1).(0); z=a.(2).(0); w=a.(3).(0)}

let is_point t = t.w=1.0
let is_vector t = t.w=0.0

let tequal a b = 
  equal_float a.x b.x &&
  equal_float a.y b.y && 
  equal_float a.z b.z &&
  equal_float a.w b.w 


(* UNARY OPS *)
let negate t = {x = t.x*.(-1.) ;  y = t.y *. (-1.) ; z = t.z *. (-1.) ; w = t.w *. (-1.)} (* ??? should the indicator (w) be negated *)
let magnitude t = sqrt (t.x *. t.x +. t.y *. t.y +. t.z *. t.z +. t.w *. t.w)
let norm t = 
  let m = magnitude t in {
    x = t.x /. m ;
    y = t.y /. m ; 
    z = t.z /. m ; 
    w = t.w /. m 
  }

(* BINARY OPS *)
let add_tup t1 t2 = {x = t1.x +. t2.x; y = t1.y +. t2.y; z = t1.z +. t2.z; w = t1.w +. t2.w}
let sub_tup t1 t2 = {x = t1.x -. t2.x; y =  t1.y -. t2.y; z = t1.z -. t2.z; w = t1.w -. t2.w}
let mult_tup tup c = {x = tup.x *. c; y = tup.y *. c; z = tup.z *. c; w = tup.w *. c}
let div_tup tup c = {x = tup.x /. c; y =  tup.y /. c; z =  tup.z /. c; w =  tup.w /. c}
let dot t1 t2 = (t1.x *. t2.x) +. (t1.y *. t2.y) +. (t1.z *. t2.z) +. (t1.w *. t2.w)
let cross a b = 
  vector 
  ((a.y *. b.z) -. (a.z *. b.y))
  ((a.z *. b.x) -. (a.x *. b.z))
  ((a.x *. b.y) -. (a.y *. b.x))


(* matrices for now are just (float array array) *)
exception UnequalDims of string;;

let m_from_list = function
  | [] | [[]] -> Array.make_matrix 0 0 0.
  | t -> Array.of_list (List.map (fun x -> Array.of_list x) t)
;;
let m_from_tuple t = [|[|t.x|] ; [|t.y|] ; [|t.z|] ; [|t.w|]|]

let mat_to_string a = 
  let inner_op acc_in xi = acc_in ^ (Printf.sprintf "%8.4f , " xi) in
  let outer_op acc_out xo = (Array.fold_left inner_op acc_out xo) ^ "\n" in
  "arr = {\n" ^ (Array.fold_left outer_op "" a) ^ "}"

let print_mat a = Array.iter (fun x -> (Array.iter (fun y -> print_float y; print_string " ") x); print_newline ();) a

let get_dims = function
  | [||] | [|[||]|] -> (0, 0)
  | t -> (Array.length t, Array.length (t.(0)))

let get_row a i = a.(i)
let get_col a i = Array.map (fun x -> x.(i)) a
let mult_arr a b = Array.map2 (fun x y -> x *. y) a b
let identity n = Array.init_matrix n n (fun x y -> if x=y then 1. else 0.)

let mequal a b = Array.for_all2 (fun x y -> Array.for_all2 (fun x y -> equal_float x y) x y) a b
let madd a b = Array.map2 (fun x y -> Array.map2 (fun x y -> x +. y) x y) a b 
let msub a b = Array.map2 (fun x y -> Array.map2 (fun x y -> x -. y) x y) a b 
let transpose a = 
  let dx, dy = get_dims a in 
  Array.init_matrix dx dy (fun i j -> a.(j).(i))
;;
let submatrix m n a =
  let ii, jj = (ref 0, ref 0) in
  let dx, dy = get_dims a in
  let res = Array.make_matrix (dx-1) (dy-1) 0. in
  for x = 0 to pred dx do 
    jj := 0;
    if x != m then begin
      for y = 0 to pred dy do 
        if y != n then begin
          res.((!ii)).((!jj)) <- a.(x).(y);
          jj := !jj + 1;
        end
      done;
      ii := !ii + 1; 
    end
  done;
  res

let rec minor i j a = submatrix i j a |> determinant
and cofactor i j a = 
  let is_odd x = (x mod 2 != 0) in
  if is_odd (i+j) then
    (-1.) *. (minor i j a)
  else
    minor i j a
and determinant a =
  let det = ref 0. in
  let dx, dy = get_dims a in
  if dx = 2 then
    (a.(0).(0) *. a.(1).(1)) -. (a.(0).(1) *. a.(1).(0))
  else begin
    for j = 0 to pred dy do 
      det := (!det +. (a.(0).(j) *. (cofactor 0 j a)))
    done;
    !det
  end
;;

let invertible a = not (equal_float (determinant a) (0.))

let invert a = 
  let dx, dy = get_dims a in
  let res = Array.make_matrix dx dy 0. in
  let det = determinant a in
  for i = 0 to pred dx do 
    for j = 0 to pred dy do 
      res.(j).(i) <- ((cofactor i j a) /. det)
    done;
  done;
  res


let matmul a b = 
  let dimA = get_dims a in
  let dimB = get_dims b in
  if (snd dimA) == (fst dimB) then begin
    let res = Array.make_matrix (fst dimA) (snd dimB) 0. in
    for i = 0 to pred (fst dimA) do
      for j = 0 to pred (snd dimB) do
        res.(i).(j) <- Array.fold_left (+.) 0. (mult_arr (get_row a i) (get_col b j));
      done;
    done;
    res
  end else
    raise (UnequalDims "dims of a and b do not match");





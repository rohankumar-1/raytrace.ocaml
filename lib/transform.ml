open Data

let translate dx dy dz = 
  let res = (identity 4) in
  res.(0).(3) <- dx;
  res.(1).(3) <- dy;
  res.(2).(3) <- dz;
  res


let scale dx dy dz = 
  let res = identity 4 in
  res.(0).(0) <- dx;
  res.(1).(1) <- dy;
  res.(2).(2) <- dz;
  res


let rotate_x rad = 
  let res = Array.make_matrix 4 4 0. in
  res.(0).(0) <- 1.;
  res.(1).(1) <- (cos rad);
  res.(1).(2) <- (-1.) *. (sin rad);
  res.(2).(1) <- (sin rad);
  res.(2).(2) <- (cos rad);
  res.(3).(3) <- 1.;
  res

let rotate_y rad = 
  let res = Array.make_matrix 4 4 0. in
  res.(0).(0) <- (cos rad);
  res.(1).(1) <- 1.;
  res.(0).(2) <- (sin rad);
  res.(2).(0) <- (-1.) *. (sin rad);
  res.(2).(2) <- (cos rad);
  res.(3).(3) <- 1.;
  res

let rotate_z rad = 
  let res = Array.make_matrix 4 4 0. in
  res.(0).(0) <- (cos rad);
  res.(0).(1) <- (-1.) *. (sin rad);
  res.(1).(0) <- (sin rad);
  res.(1).(1) <- (cos rad);
  res.(2).(2) <- 1.;
  res.(3).(3) <- 1.;
  res

let shear dxy dxz dyx dyz dzx dzy = 
  let res = identity 4 in
  res.(0).(1) <- dxy;
  res.(1).(0) <- dyx;
  res.(0).(2) <- dxz;
  res.(2).(0) <- dzx;
  res.(1).(2) <- dyz;
  res.(2).(1) <- dzy;
  res


let chain_transforms lst = 
  let rec aux acc = function
    | [] -> identity 4
    | t :: [] -> matmul t acc
    | t :: u -> aux (matmul t acc) u 
  in aux (identity 4) lst
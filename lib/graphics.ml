open Data

class sphere = object (self)
  val mutable id = 0;
  val mutable transform = (identity 4 : float array array);

  initializer 
    id <- (Oo.id self)

  method set_transform t =
    transform <- t
  method get_transform = 
    transform
end

type intersection = {
  obj : int;
  t : float
}

let hit lst = 
  let rec aux smallest = function 
    | [] -> smallest
    | a :: u -> 
      if a.t < 0. || a.t > smallest.t then 
        aux smallest u
      else begin
        aux a u
      end;
    in aux {t=Float.max_float; obj=(-1)} lst
  ;;

module Ray : sig
  (* TYPES DEFINED BY RAY*)
  type ray

  (* FUNCTIONS TO USE WITH RAY *)
  val make_ray : p:tuple -> v:tuple -> ray
  val position : ray -> float -> tuple
  val intersect : ray -> sphere -> intersection list
  val transform : ray -> float array array -> ray
end = struct
  type ray = {
    origin : tuple;
    dir : tuple;
  }

  let make_ray ~p ~v = {origin=p; dir=v}
  let position r t = add_tup r.origin (mult_tup r.dir t)
  let transform r m = {
      origin=(t_from_mat (matmul m (m_from_tuple (r.origin)))); 
      dir=(t_from_mat (matmul m (m_from_tuple (r.dir))))
      }
  let intersect r_og (s:sphere) =
    let r = transform r_og (invert s#get_transform) in
    let sphere_to_ray = sub_tup r.origin (point 0. 0. 0.) in
    let a = dot r.dir r.dir in
    let b = 2. *. (dot r.dir sphere_to_ray) in 
    let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
    let discriminant = (Float.pow b 2.) -. (4. *. a *. c) in
    if discriminant < 0. then
      []
    else begin
      [
        {t=(((Float.neg b) -. (sqrt discriminant)) /. (2. *. a)); obj=(Oo.id s)};
        {t=(((Float.neg b) +. (sqrt discriminant)) /. (2. *. a)); obj=(Oo.id s)}
      ]
    end 
    

end







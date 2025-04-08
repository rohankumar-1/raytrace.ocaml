open Data
open Draw
open Constants

(**
For all shapes, we share the following members and functions:
- member: transform
- member: material
- setters and getters for transform and material

For each shape, we need the following functions implemented seperately
- local_normal_at
- local_intersect
*)


module Ray = struct
  type ray = { origin : tuple; dir : tuple; }

  let make ~p ~v = {origin=p; dir=v}
  let get_dir r = r.dir
  let get_origin r = r.origin
  let position r t = tadd r.origin (tmult r.dir t)
  let transform r m = 
      { origin=(tup_from_mat (matmul m (mat_from_tup (r.origin)))); 
        dir=(tup_from_mat (matmul m (mat_from_tup (r.dir)))) }

end


class virtual shape ?(tf=(identity 4)) ?(mat=make_material ()) () = object (self)
  val mutable transform = tf;
  val mutable material = mat; (* default color is white *)
  val mutable saved_ray : Ray.ray option = None;

  method set_transform t = transform <- t
  method get_transform = transform
  method set_material m = material <- m
  method get_material = material
  method get_saved_ray = Option.get saved_ray

  method virtual local_normal_at : tuple -> tuple
  method virtual local_intersect : Ray.ray -> float list

  method normal_at p =
    let inv_tf = invert transform in 
    let obj_pt = tup_from_mat (matmul inv_tf (mat_from_tup p)) in 
    let obj_norm = self#local_normal_at obj_pt in 
    let world_norm = matmul (transpose inv_tf) (mat_from_tup obj_norm) in 
    world_norm.(3).(0) <- 0.;
    norm (tup_from_mat world_norm)

end


class sphere = object
  inherit shape ()

  method local_normal_at pt = 
    tsub pt (point 0. 0. 0.)

  method local_intersect local_r = 
    saved_ray <- Some local_r;
    let sphere_to_ray = tsub local_r.origin (point 0. 0. 0.) in
    let a = dot local_r.dir local_r.dir in
    let b = 2. *. (dot local_r.dir sphere_to_ray) in 
    let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
    let discriminant = (Float.pow b 2.) -. (4. *. a *. c) in
    if discriminant < 0. then []
    else begin
      [((Float.neg b) -. (sqrt discriminant)) /. (2. *. a);
       ((Float.neg b) +. (sqrt discriminant)) /. (2. *. a)]
  end 
end


class plane = object 
  inherit shape ()
  
  method local_normal_at _ = (vector 0. 1. 0.)

  method local_intersect local_r = 
    saved_ray <- Some local_r;
    if Float.abs (local_r.dir.y) < _EPSILON then []
    else [(-.local_r.origin.y) /. local_r.dir.y]

end


class cube = object
  inherit shape ()

  method local_normal_at pt =
    let maxc = max (max (abs_float pt.x) (abs_float pt.y)) (abs_float pt.z) in 

    if equal_float maxc (abs_float pt.x) then vector pt.x 0. 0. 
    else if equal_float maxc (abs_float pt.y) then vector 0. pt.y 0.
    else vector 0. 0. pt.z

  method local_intersect local_r = 
    let check_axis org dir = 
      let tmin_num, tmax_num = ((-1.) -. org, 1. -. org) in 
      let tmin, tmax = 
        if abs_float dir >= _EPSILON then begin 
          (tmin_num /. dir, tmax_num /. dir)
        end else begin
          (tmin_num *. infinity, tmax_num *. infinity)
        end in
      if tmin > tmax then (tmax, tmin) else (tmin, tmax)
    in
    let dir, org = (Ray.get_dir local_r, Ray.get_origin local_r) in 
    let xtmin, xtmax = check_axis org.x dir.x in 
    let ytmin, ytmax = check_axis org.y dir.y in 
    let ztmin, ztmax = check_axis org.z dir.z in 
    let tmin = max (max xtmin ytmin) ztmin in 
    let tmax = min (min xtmax ytmax) ztmax in
    if tmin > tmax then [] else [tmin;tmax]

end




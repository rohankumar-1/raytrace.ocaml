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


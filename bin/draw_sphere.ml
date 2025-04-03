open Raytrace.Draw
open Raytrace.Graphics
open Raytrace.Data





let () = 
  (* ray_origin is where our "camera" is from, i.e. where the rays will start from *)
  let ray_origin = point 0. 0. (-5.) in

  (* wall_z is the location of our wall, i.e. how far in the z direction is the shadow? *)
  let wall_z = 10.0 in
  (* wall_size is the x and y size of the wall, as anchored at z=wall_z *)
  let wall_size = 7.0 in  

  (* can_pixs is the size of the canvas is pixels *)
  let can_pixs = 800 in 
  (* the canvas is the grid representation of a portion of the wall *)
  let can = make_canvas ~h:can_pixs ~w:can_pixs in
  (* now, we can find how large a pixel is (in world space units) i.e. how much of the wall is capture by a pixel *)
  let pixel_sz = wall_size /. (float_of_int can_pixs) in
  
  (* the direction we are looking is directly at the center of the sphere, so half of the wall will be to the left, half to the right *)
  let half = wall_size /. 2. in 

  (* sphere is the object we are capturing the silhouette of, and color is the color of that silhouette *)
  let col = build_color 1. 0. 0. in 
  let sph = new sphere in

  (* now, the routine to project each ray *)
  for y = 0 to pred can_pixs do 
    let world_y = half -. (pixel_sz*.(float_of_int y)) in 
    for x = 0 to pred can_pixs do 
      (* world_x and world_y are the coordinates of the target pixel IRL, and pos describes this point on the wall *)
      let world_x = (Float.neg half) +. (pixel_sz*.(float_of_int x)) in 
      let pos = point world_x world_y wall_z in 

      (* now, we get the ray, and intersect it with the sphere *)
      let r = Ray.make_ray ~p:ray_origin ~v:(norm (sub_tup pos ray_origin)) in
      let xs = Ray.intersect r sph in 

      (* if there is an intersection (hit xs != 0), then we know that this pixel is part of the silhoutte *)
      if (hit xs).obj != (-1) then begin
        can.grid.(x).(y) <- Color col ;
      end
    done;
  done;

  let oc = open_out "output/sphere_silhouette.ppm" in
    write_canvas_P6 ~oc:oc ~can:can;


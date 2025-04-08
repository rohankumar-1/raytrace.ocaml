open Data
open Draw
open Transform
open Shapes
open Constants

type intersection = { obj:shape ref option ; t : float }
type point_light = { intensity:color ; pos:tuple }
type world = { light:point_light ; objects:sphere list }

type info = {
  dist: float;
  obj : sphere;
  pt : tuple;
  eyev: tuple;
  normv: tuple;
  inside: bool;
  over_pt: tuple;
  reflectv: tuple;
}

type camera = {
  hsize: int;
  vsize: int;
  fov: float;
  tf: float array array ref;
  pixel_size: float;
  half_width: float;
  half_height: float;
}

let hit lst = 
  let rec aux smallest = function 
    | [] -> smallest
    | a :: u -> 
      if a.t < 0. || a.t > smallest.t then aux smallest u
      else aux a u
    in aux {t=Float.max_float; obj=None} lst
  
let intersect r (s:shape) =
  let local_r = Ray.transform r (invert s#get_transform) in
  let ts = s#local_intersect local_r in 
  let s_ref = Some (ref s) in 
  let cover = function 
    | [x ; y] -> [{t=x; obj=s_ref}; {t=y; obj=s_ref}]
    | [x] -> [{t=x; obj=s_ref}]
    | _ -> []
  in cover ts

let intersect_world w r = 
  let rec aux = function
    | [] -> []
    | a :: u -> List.append (intersect r a) (aux u)
  in let res = aux w.objects in
  List.sort (fun x y -> if x.t < y.t then (-1) else if x.t > y.t then 1 else 0) res
  
let get_shape (i:intersection) = !(Option.get i.obj)

let make_light c p = {intensity=c ; pos=p}

let get_pattern obj_tf (pt:tuple) = function
  | Plain c -> c
  | Stripe (tf, f) | Checker (tf, f) -> 
    (
      let obj_pt = matmul (invert obj_tf) (mat_from_tup pt) in 
      let pattern_pt = matmul (invert tf) obj_pt in 
      f (tup_from_mat pattern_pt)
    )


let lighting (obj:shape) light pt eyev normv in_shadow =
  let mat = obj#get_material in 
  let c = get_pattern (obj#get_transform) pt mat.pattern in

  let eff_col = hadamard c light.intensity in
  let lightv = norm (tsub light.pos pt) in
  let ambient = cmult eff_col mat.amb in
  let light_dot_norm = dot lightv normv in

  let diff, spec =
    if light_dot_norm < 0. || in_shadow then
      (make_color 0. 0. 0., make_color 0. 0. 0.)
    else begin 
      let diff = cmult eff_col (mat.dif *. light_dot_norm) in
      let reflectv = reflect (tneg lightv) normv in
      let reflect_dot_eye = dot reflectv eyev in
      let spec =
        if reflect_dot_eye <= 0. then make_color 0. 0. 0.
        else begin
          let factor = Float.pow reflect_dot_eye mat.shine in
          cmult light.intensity (mat.spec *. factor)
        end
      in
      (diff, spec)
    end
  in
  cadd (cadd ambient diff) spec

let default_world () = 
  let sph1, sph2 = (new sphere, new sphere) in
  sph1#set_material (make_material ~sp:0.2 ~di:0.7 ~pat:(Plain (make_color 0.8 1.0 0.6)) ());
  sph2#set_transform (scale 0.5 0.5 0.5);
  {
  light = make_light (make_color 1. 1. 1.) (point (-10.) 10. (-10.));
  objects = [sph1; sph2;]
  }

let prepare_computations i r = 
  let s = get_shape i in 
  let p = Ray.position r i.t in 
  let ev = tneg (Ray.get_dir r) in 
  let temp_nv = s#normal_at p in 
  let ins = (dot ev temp_nv) < 0. in
  let nv = if ins then tneg temp_nv else temp_nv in 

  {
    dist=   i.t;
    obj=    s;
    pt=     p;
    eyev=   ev;
    normv=  nv;
    inside= ins;
    over_pt= tadd p (tmult nv _EPSILON);
    reflectv= reflect (Ray.get_dir r) nv;
  }

let pattern_get = function
  | Plain c -> c
  | _ -> _BLACK

let is_shadowed world pt (h_obj:intersection) = 
  let v = tsub (world.light.pos) pt in 
  let dist, dir = (magnitude v, norm v) in 
  let h = hit (intersect_world world (Ray.make ~p:pt ~v:dir)) in 
  (h.obj != None) && (get_shape (h) != get_shape (h_obj)) && (h.t <= dist)

let rec color_at w r remaining = 
  let h = hit (intersect_world w r) in 
  if h.obj != None then begin
    let comp = prepare_computations h r in 
    let shadowed = is_shadowed w comp.over_pt h in 
    let surface = lighting comp.obj w.light comp.over_pt comp.eyev comp.normv shadowed in 
    let reflected = reflected_color w comp remaining in 
    cadd surface reflected
  end else _BLACK

and reflected_color world comps remaining = 
  let refl = comps.obj#get_material.reflective in
  if refl = 0.0 then _BLACK
  else begin
    let reflect_ray = Ray.make ~p:(comps.over_pt) ~v:(comps.reflectv) in
    let c = color_at world reflect_ray (remaining - 1) in 
    cmult c refl
  end

let view_transform fromv tov upv = 
  let forwardv = norm (tsub tov fromv) in 
  let leftv = cross forwardv (norm upv) in 
  let true_upv = cross leftv forwardv in 
  let neg_for = tneg forwardv in 
  let mat = 
  [|
    [|leftv.x ;     leftv.y ;     leftv.z;    0.  |];
    [|true_upv.x ;  true_upv.y ;  true_upv.z; 0.  |]; 
    [|neg_for.x ;   neg_for.y ;   neg_for.z;  0.  |];
    [|0.;           0.;           0.;         1.  |];
  |] in 
  matmul mat (translate (-.fromv.x) (-.fromv.y) (-.fromv.z))


let make_camera hs vs fov = 
  let half_view = tan (fov /. 2.) in 
  let aspect = (float_of_int hs) /. (float_of_int vs) in 
  let adjust_bool = (aspect >= 1.) in 
  let half_w, half_h = if adjust_bool then (half_view, half_view /. aspect) else (half_view *. aspect, half_view) in 
  {
    hsize=hs;
    vsize=vs;
    fov=fov;
    tf=(ref (identity 4));
    pixel_size= (half_w *. 2.) /. (float_of_int hs);
    half_width= half_w;
    half_height= half_h;
  }

let ray_for_pixel cam px py = 
  let x_offset = (px +. 0.5) *. cam.pixel_size in 
  let y_offset = (py +. 0.5) *. cam.pixel_size in 
  let world_x = cam.half_width -. x_offset in 
  let world_y = cam.half_height -. y_offset in 
  let pixel = tup_from_mat (matmul (invert !(cam.tf)) (mat_from_tup (point world_x world_y (-1.)))) in 
  let origin = tup_from_mat (matmul (invert !(cam.tf)) (mat_from_tup (point 0. 0. 0.))) in 
  let direction = norm (tsub pixel origin) in 
  Ray.make ~p:origin ~v:direction 

let render (cam:camera) (world:world) ?(reflect_recurse=0) ()= 
  let img = make_canvas ~w:cam.hsize ~h:cam.vsize in 
  for y = 0 to pred cam.vsize do 
    for x = 0 to pred cam.hsize do 
      (* Printf.printf "(%d,%d): " x y; *)
      let r = ray_for_pixel cam (float_of_int x) (float_of_int y) in 
      let c = color_at world r reflect_recurse in 
      img.grid.(x).(y) <- c;
    done;
  done;
  img


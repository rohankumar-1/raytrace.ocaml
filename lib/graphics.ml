open Data
open Draw
open Transform
open Shapes

type intersection = { obj:sphere ref option ; t : float }
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
  
let get_sphere (i:intersection) = !(Option.get i.obj)

let make_light c p = {intensity=c ; pos=p}

let lighting mat light pt eyev normv in_shadow =
  let eff_col = hadamard mat.col light.intensity in
  let lightv = norm (tsub light.pos pt) in
  let ambient = cmult eff_col mat.amb in
  let light_dot_norm = dot lightv normv in

  let diff, spec =
    if light_dot_norm < 0. || in_shadow then
      (make_color 0. 0. 0., make_color 0. 0. 0.)
    else
      let diff = cmult eff_col (mat.dif *. light_dot_norm) in
      let reflectv = reflect (tmult lightv (-1.)) normv in
      let reflect_dot_eye = dot reflectv eyev in
      let spec =
        if reflect_dot_eye <= 0. then make_color 0. 0. 0.
        else
          let factor = Float.pow reflect_dot_eye mat.shine in
          cmult light.intensity (mat.spec *. factor)
      in
      (diff, spec)
  in
  Color (cadd (cadd ambient diff) spec)

let default_world () = 
  let sph1, sph2 = (new sphere, new sphere) in
  sph1#set_material (make_material ~sp:0.2 ~di:0.7 ~c:(make_color 0.8 1.0 0.6) ());
  sph2#set_transform (scale 0.5 0.5 0.5);
  {
  light = make_light (make_color 1. 1. 1.) (point (-10.) 10. (-10.));
  objects = [sph1; sph2;]
  }

let prepare_computations i r = 
  let s = get_sphere i in 
  let p = Ray.position r i.t in 
  let ev = tneg (Ray.get_dir r) in 
  let nv = s#normal_at p in 
  let ins = (dot ev nv) < 0. in
  {
    dist=   i.t;
    obj=    s;
    pt=     p;
    eyev=   ev;
    normv=  if ins then tneg nv else nv;
    inside= ins;
    over_pt= tadd p (tmult nv 0.001);
  }

let is_shadowed world pt = 
  let v = tsub (world.light.pos) pt in 
  let dist, dir = (magnitude v, norm v) in 
  let r = Ray.make ~p:pt ~v:dir in 
  let xs = intersect_world world r in 
  let h = hit xs in 
  (h.obj != None && h.t <= dist)


let color_at w r = 
  let xs = intersect_world w r in 
  let h = hit xs in 
  if h.obj != None then begin
    let comp = prepare_computations (hit xs) r in 
    lighting (comp.obj#get_material) w.light comp.pt comp.eyev comp.normv (is_shadowed w comp.over_pt)
  end else Blank


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

let render (cam:camera) (world:world) = 
  let img = make_canvas ~w:cam.hsize ~h:cam.vsize in 
  for y = 0 to pred cam.vsize do 
    for x = 0 to pred cam.hsize do 
      let r = ray_for_pixel cam (float_of_int x) (float_of_int y) in 
      let c = color_at world r in 
      img.grid.(x).(y) <- c;
    done;
  done;
  img


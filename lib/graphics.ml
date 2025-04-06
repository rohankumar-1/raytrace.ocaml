open Data
open Draw
open Transform

type material = {
  col: color;
  amb: float;
  dif: float;
  spec: float;
  shine: float;
}

let make_material ?(am=0.1) ?(di=0.9) ?(sp=0.9) ?(sh=200.0) ?(c=(make_color 1. 1. 1.)) () = {
  col=c;
  amb=am;
  dif=di;
  spec=sp;
  shine=sh
}   

class sphere = object (self)
  val mutable id = 0;
  val mutable transform = (identity 4 : float array array);
  val mutable material = (make_material ()); (* default color is white *)

  initializer 
    id <- (Oo.id self)

  method set_transform t =
    transform <- t
  
  method get_transform = 
    transform

  method set_material m = 
    material <- m

  method get_material = 
    material

  method normal_at p = 
    let inv_transform = invert transform in
    let object_pt = matmul inv_transform (m_from_tuple p) |> t_from_mat in
    let object_norm = sub_tup object_pt (point 0. 0. 0.) in
    let world_norm = matmul (transpose inv_transform) (m_from_tuple object_norm) in
    world_norm.(3).(0) <- 0.;
    norm (t_from_mat world_norm)

end

type intersection = {
  obj : sphere ref option;
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
    in aux {t=Float.max_float; obj=None} lst
  ;;

let get_sphere i = !(Option.get i.obj)

module Ray : sig
  (* TYPES DEFINED BY RAY*)
  type ray

  (* FUNCTIONS TO USE WITH RAY *)
  val make_ray : p:tuple -> v:tuple -> ray
  val position : ray -> float -> tuple
  val intersect : ray -> sphere -> intersection list
  val transform : ray -> float array array -> ray
  val get_dir : ray -> tuple
  val get_origin : ray -> tuple
end = struct
  type ray = {
    origin : tuple;
    dir : tuple;
  }

  let make_ray ~p ~v = {origin=p; dir=v}

  let get_dir r = r.dir
  let get_origin r = r.origin
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
      let s_ref = ref s in 
      [
        {t=(((Float.neg b) -. (sqrt discriminant)) /. (2. *. a)); obj=(Some s_ref)};
        {t=(((Float.neg b) +. (sqrt discriminant)) /. (2. *. a)); obj=(Some s_ref)}
      ]
    end 
end

type point_light = {
  intensity : color;
  pos : tuple
}

let make_light c p = {intensity=c ; pos=p}

let lighting mat light pt eyev normv in_shadow = 
  let eff_col = hadamard mat.col light.intensity in 
  let lightv = norm (sub_tup light.pos pt) in 
  let ambient = mult_clr eff_col mat.amb in 
  let light_dot_norm = dot lightv normv in 
  
  let diff = ref (make_color 0. 0. 0.) in
  let spec = ref (make_color 0. 0. 0.) in 

  (* if light_dot_norm < 0, then light is on other side of surface, so we can leave diff and spec contributions as black *)
  if light_dot_norm >= 0. && (not in_shadow) then begin
    diff := (mult_clr eff_col (mat.dif *. light_dot_norm));
    let reflectv = reflect (mult_tup lightv (-1.)) normv in 
    let reflect_dot_eye = dot reflectv eyev in 

    if reflect_dot_eye > 0. then begin
      let factor = Float.pow reflect_dot_eye mat.shine in 
      spec := (mult_clr light.intensity (mat.spec *. factor));
      Color (add_clr (add_clr ambient !diff) !spec)
    end else begin
      Color (add_clr (add_clr ambient !diff) !spec)
    end
  end else begin
    Color (add_clr (add_clr ambient !diff) !spec)
  end
  


type world = {
  light : point_light ;
  objects : sphere list;
}

let default_world () = 
  let sph1 = new sphere in
  let sph2 = new sphere in 
  sph1#set_material (make_material ~sp:0.2 ~di:0.7 ~c:(make_color 0.8 1.0 0.6) ());
  sph2#set_transform (scale 0.5 0.5 0.5);
  {
  light = make_light (make_color 1. 1. 1.) (point (-10.) 10. (-10.));
  objects = [
      sph1;
      sph2;
    ]
  }


let intersect_world w r = 
  let intersect_sph r sph = 
    let r = Ray.transform r (invert sph#get_transform) in
    let sphere_to_ray = sub_tup (Ray.get_origin r) (point 0. 0. 0.) in
    let dir = Ray.get_dir r in
    let a = dot dir dir in
    let b = 2. *. (dot dir sphere_to_ray) in 
    let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
    let discriminant = (Float.pow b 2.) -. (4. *. a *. c) in
    if discriminant < 0. then
      []
    else begin
      let sph_ref = ref sph in 
      [
        {t=(((Float.neg b) -. (sqrt discriminant)) /. (2. *. a)); obj=(Some sph_ref)};
        {t=(((Float.neg b) +. (sqrt discriminant)) /. (2. *. a)); obj=(Some sph_ref)}
      ]
    end 
  in 
  let rec aux = function
    | [] -> []
    | a :: u -> List.append (intersect_sph r a) (aux u)
  in let res = aux w.objects in
  List.sort (fun x y -> if x.t < y.t then -1 else if x.t > y.t then 1 else 0) res

type info = {
  dist: float;
  obj : sphere;
  pt : tuple;
  eyev: tuple;
  normv: tuple;
  inside: bool;
  over_pt: tuple;
}

let prepare_computations i r = 
  let s = get_sphere i in 
  let p = Ray.position r i.t in 
  let ev = mult_tup (Ray.get_dir r) (-1.) in 
  let nv = s#normal_at p in 
  let ins = (dot ev nv) < 0. in
  {
    dist=   i.t;
    obj=    s;
    pt=     p;
    eyev=   ev;
    normv=  if ins then mult_tup nv (-1.) else nv;
    inside= ins;
    over_pt= add_tup p (mult_tup nv 0.001);
  }




let is_shadowed world pt = 
  let v = sub_tup (world.light.pos) pt in 
  let dist = magnitude v in 
  let dir = norm v in 

  let r = Ray.make_ray ~p:pt ~v:dir in 
  let xs = intersect_world world r in 
  let h = hit xs in 
  (h.obj != None && h.t <= dist)

let color_at w r = 
  let xs = intersect_world w r in 
  if List.length xs > 0 then begin
    let comp = prepare_computations (List.nth xs 0) r in 
    lighting (comp.obj#get_material) w.light comp.pt comp.eyev comp.normv (is_shadowed w comp.over_pt)
  end else Blank


let view_transform fromv tov upv = 
  let forwardv = norm (sub_tup tov fromv) in 
  let leftv = cross forwardv (norm upv) in 
  let true_upv = cross leftv forwardv in 
  let neg_for = mult_tup forwardv (-1.) in 
  let mat = 
  [|
    [|leftv.x ;     leftv.y ;     leftv.z;    0.  |];
    [|true_upv.x ;  true_upv.y ;  true_upv.z; 0.  |]; 
    [|neg_for.x ;   neg_for.y ;   neg_for.z;  0.  |];
    [|0.;           0.;           0.;         1.  |];
  |] in 
  matmul mat (translate (-.fromv.x) (-.fromv.y) (-.fromv.z))
  

type camera = {
  hsize: int;
  vsize: int;
  fov: float;
  tf: float array array ref;
  pixel_size: float;
  half_width: float;
  half_height: float;
}

let make_camera hs vs fov = 
  let half_view = tan (fov /. 2.) in 
  let aspect = (float_of_int hs) /. (float_of_int vs) in 
  let adjust_bool = (aspect >= 1.) in 
  let half_w = if adjust_bool then half_view else half_view *. aspect in 
  let half_h = if adjust_bool then half_view /. aspect else half_view in  
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

  let pixel = t_from_mat (matmul (invert !(cam.tf)) (m_from_tuple (point world_x world_y (-1.)))) in 
  let origin = t_from_mat (matmul (invert !(cam.tf)) (m_from_tuple (point 0. 0. 0.))) in 
  let direction = norm (sub_tup pixel origin) in 
  Ray.make_ray ~p:origin ~v:direction 


let render cam world = 
  let img = make_canvas ~w:cam.hsize ~h:cam.vsize in 

  for y = 0 to pred cam.vsize do 
    for x = 0 to pred cam.hsize do 
      let r = ray_for_pixel cam (float_of_int x) (float_of_int y) in 
      let c = color_at world r in 
      img.grid.(x).(y) <- c;
    done;
  done;
  img









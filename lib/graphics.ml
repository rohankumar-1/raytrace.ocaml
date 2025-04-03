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

let make_material ?(am=0.1) ?(di=0.9) ?(sp=0.9) ?(sh=200.0) ?(c=(build_color 1. 1. 1.)) () = {
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

  method set_material m= 
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
end = struct
  type ray = {
    origin : tuple;
    dir : tuple;
  }

  let make_ray ~p ~v = {origin=p; dir=v}

  let get_dir r = r.dir
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

let lighting mat light pt eyev normv = 
  let eff_col = hadamard mat.col light.intensity in 
  let lightv = norm (sub_tup light.pos pt) in 
  let ambient = mult_clr eff_col mat.amb in 
  let light_dot_norm = dot lightv normv in 
  
  let diff = ref (build_color 0. 0. 0.) in
  let spec = ref (build_color 0. 0. 0.) in 

  (* if light_dot_norm < 0, then light is on other side of surface, so we can leave diff and spec contributions as black *)
  if light_dot_norm >= 0. then begin
    diff := (mult_clr eff_col (mat.dif *. light_dot_norm));
    let reflectv = reflect (mult_tup lightv (-1.)) normv in 
    let reflect_dot_eye = dot reflectv eyev in 

    if reflect_dot_eye > 0. then begin
      let factor = Float.pow reflect_dot_eye mat.shine in 
      spec := (mult_clr light.intensity (mat.spec *. factor));
      add_clr (add_clr ambient !diff) !spec
    end else begin
      add_clr (add_clr ambient !diff) !spec
    end
  end else begin
    add_clr (add_clr ambient !diff) !spec
  end
  








(*
*)



(* class tuple (x, y ,z ,w) = 
object
  val x : float = x
  val y : float = y
  val z : float = z
  val w : float = w

  method gx = x
  method gy = y
  method gz = z
  method gw = w
  
  method is_point = 
    if w=1. then true else false
  method is_vector = 
    if w=0. then true else false

end

let point (x, y, z) = new tuple (x, y, z, 1.)
let vector (x, y, z) = new tuple (x, y, z, 0.) *)

type tuple = {x:float ; y:float ; z:float ; w:float}
let point (x,y,z) = {x=x ; y=y ; z=z ; w=1.0} 
let vector (x,y,z) = {x=x; y=y ; z=z ; w=0.0} 

let is_point t = t.w=1.0
let is_vector t = t.w=0.0


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
let cross a b = vector (
  (a.y *. b.z) -. (a.z *. b.y),
  (a.z *. b.x) -. (a.x *. b.z),
  (a.x *. b.y) -. (a.y *. b.x)
)

(* OPERATOR BINDINGS *)
let ( ~~ ) = negate
let ( ++ ) = add_tup
let ( -- ) = sub_tup
let ( // ) = div_tup
let ( *** ) = mult_tup
let ( *@ ) = cross
let ( *- ) = dot
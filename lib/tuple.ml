(*
*)



class tuple (x, y ,z ,w) = 
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
let vector (x, y, z) = new tuple (x, y, z, 0.)

(* UNARY OPS *)
let negate t = new tuple (t#gx *. (-1.), t#gy *. (-1.), t#gz *. (-1.), t#gw *. (-1.)) (* ??? should the indicator (w) be negated *)
let magnitude t = sqrt (t#gx *. t#gx +. t#gy *. t#gy +. t#gz *. t#gz +. t#gw *. t#gw)
let norm t = 
  let m = magnitude t in 
  new tuple (t#gx /. m, t#gy /. m, t#gz /. m, t#gw /. m)

(* BINARY OPS *)
let add_tup t1 t2 = new tuple (t1#gx +. t2#gx, t1#gy +. t2#gy, t1#gz +. t2#gz, t1#gw +. t2#gw)
let sub_tup t1 t2 = new tuple (t1#gx -. t2#gx, t1#gy -. t2#gy, t1#gz -. t2#gz, t1#gw -. t2#gw)
let mult_tup tup c = new tuple (tup#gx *. c, tup#gy *. c, tup#gz *. c, tup#gw *. c)
let div_tup tup c = new tuple (tup#gx /. c, tup#gy /. c, tup#gz /. c, tup#gw /. c)
let dot t1 t2 = (t1#gx *. t2#gx) +. (t1#gy *. t2#gy) +. (t1#gz *. t2#gz) +. (t1#gw *. t2#gw)
let cross a b = vector (
  (a#gy *. b#gz) -. (a#gz *. b#gy),
  (a#gz *. b#gx) -. (a#gx *. b#gz),
  (a#gx *. b#gy) -. (a#gy *. b#gx)
)

(* OPERATOR BINDINGS *)
let ( ~~ ) = negate
let ( ++ ) = add_tup
let ( -- ) = sub_tup
let ( // ) = div_tup
let ( *** ) = mult_tup
let ( *@ ) = cross
let ( *- ) = dot
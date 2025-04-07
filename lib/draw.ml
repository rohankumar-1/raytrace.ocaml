open Data

(***************************************************)
(* TYPE DEFINITIONS *)
(***************************************************)
type color = {red:float ; green:float ; blue:float}

type pattern = 
  | Stripe of float array array * (tuple -> color)
  | Checker of float array array * (tuple -> color)
  | Plain of color

type material = {
  pattern: pattern;
  amb: float;
  dif: float;
  spec: float;
  shine: float;
  (* reflective: float; *)
}

type canvas = {
  height: int;
  width: int;
  grid: color array array
}


(***************************************************)
(* BUILDER FUNCTIONS *)
(***************************************************)
let make_color r g b = {red=r; green=g; blue=b}
let _WHITE = (make_color 1. 1. 1.)
let _BLACK = (make_color 0. 0. 0.)
let _RED = (make_color 1. 0. 0.)
let _GREEN = (make_color 0.0 1. 0.0)
let _BLUE = (make_color 0. 0. 1.)

let make_material ?(am=0.1) ?(di=0.9) ?(sp=0.9) ?(sh=200.0) ?(pat=Plain (make_color 1. 1. 1.)) () = {
  pattern=pat;
  amb=am;
  dif=di;
  spec=sp;
  shine=sh;
  (* reflective=reflect; *)
}   
let make_canvas ~w ~h = {width=w; height=h; grid= Array.make_matrix w h _BLACK}

let make_plain c = Plain c

let make_stripe (ca:color) (cb:color) tf =
  Stripe (
    tf,
    fun pt -> if (int_of_float (floor (pt.x)) mod 2 = 0) then ca else cb
  )

let make_checked (ca:color) (cb:color) tf = 
  Checker (
    tf,
    fun pt -> if (int_of_float ((floor pt.x) +. (floor pt.y) +. (floor pt.z)) mod 2) = 0 then ca else cb
  )


(***************************************************)
(* COLOR OPS *)
(***************************************************)
let cadd t1 t2 = {red = t1.red +. t2.red; green = t1.green +. t2.green; blue = t1.blue +. t2.blue}
let csub t1 t2 = {red = t1.red -. t2.red; green = t1.green -. t2.green; blue = t1.blue -. t2.blue}
let cmult t c = {red = t.red *. c; green = t.green *. c; blue = t.blue *. c}
let hadamard t1 t2 = {red = t1.red *. t2.red; green = t1.green *. t2.green; blue = t1.blue *. t2.blue}
let cequal a b = (equal_float a.red b.red) && (equal_float a.green b.green) && (equal_float a.blue b.blue)


(***************************************************)
(* PATTERN OPS *)
(***************************************************)


(***************************************************)
(* HELPER FUNCTIONS *)
(***************************************************)
let scale_clr x = 
  let scaled = 255. *. x in
  if scaled > 255. then 255
  else if scaled < 0. then 0
  else int_of_float scaled

let col_to_string_P3 c =  Printf.sprintf "%-3d %-3d %-3d" (scale_clr c.red) (scale_clr c.green) (scale_clr c.blue)


(***************************************************)
(* PRINTING / IO FUNCTIONS *)
(***************************************************)
let print_clr a = Printf.printf "color is [r: %4.2f g: %4.2f b: %4.2f]" a.red a.green a.blue

let write_canvas_P3 ~oc ~can =
  (* following line writes initial text (format, dims, color scale) to file*)
  Printf.fprintf oc "P3\n%d %d\n255\n" can.width can.height;
  for y = 0 to pred can.height do
    for x = 0 to pred can.width do 
      Printf.fprintf oc "%-12s  " (col_to_string_P3 can.grid.(x).(y));
    done;
    Printf.fprintf oc "\n,";
  done;
  flush oc

let write_canvas_P6 ~oc ~can =
  Printf.fprintf oc "P6 %d %d 255\n" can.width can.height;
  for y = 0 to pred can.height do
    for x = 0 to pred can.width do
      let c = can.grid.(x).(y) in 
      output_char oc (char_of_int (scale_clr c.red));
      output_char oc (char_of_int (scale_clr c.green));
      output_char oc (char_of_int (scale_clr c.blue));
    done;
  done;
  output_char oc '\n';
  flush oc


  
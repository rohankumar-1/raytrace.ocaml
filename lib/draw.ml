open Util

type color = {red:float ; green:float ; blue:float}
let build_color r g b = {red=r; green=g; blue=b}

let add_clr t1 t2 = {red = t1.red +. t2.red; green = t1.green +. t2.green; blue = t1.blue +. t2.blue}
let sub_clr t1 t2 = {red = t1.red -. t2.red; green = t1.green -. t2.green; blue = t1.blue -. t2.blue}
let mult_clr t c = {red = t.red *. c; green = t.green *. c; blue = t.blue *. c}
let hadamard t1 t2 = {red = t1.red *. t2.red; green = t1.green *. t2.green; blue = t1.blue *. t2.blue}
let equal_clr a b = equal_float a.red b.red && equal_float a.green b.green && equal_float a.blue b.blue


type pixel =
  | Color of color
  | Blank

type canvas = {
  height: int;
  width: int;
  grid: pixel array array
}

let scale_format_pixel pixel = 
  match pixel with
  | Blank -> "0 0 0"
  | Color c -> Printf.sprintf "%d %d %d" (scale c.red) (scale c.green) (scale c.blue)

let make_canvas ~h ~w = {height=h; width=w; grid= Array.make_matrix h w Blank}

let write_canvas ~oc ~can =
  (* following line writes initial text (format, dims, color scale) to file*)
  Printf.fprintf oc "P3\n%d %d\n255\n" can.height can.width;
  Array.iter (fun x -> Array.iter (fun y -> Printf.fprintf oc "%s " (scale_format_pixel y);) x ; Printf.fprintf oc "%s\n" "") can.grid
;; 

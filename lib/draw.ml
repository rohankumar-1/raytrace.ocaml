open Data

type color = {red:float ; green:float ; blue:float}
let make_color r g b = {red=r; green=g; blue=b}

let add_clr t1 t2 = {red = t1.red +. t2.red; green = t1.green +. t2.green; blue = t1.blue +. t2.blue}
let sub_clr t1 t2 = {red = t1.red -. t2.red; green = t1.green -. t2.green; blue = t1.blue -. t2.blue}
let mult_clr t c = {red = t.red *. c; green = t.green *. c; blue = t.blue *. c}
let hadamard t1 t2 = {red = t1.red *. t2.red; green = t1.green *. t2.green; blue = t1.blue *. t2.blue}
let cequal a b = (equal_float a.red b.red) && (equal_float a.green b.green) && (equal_float a.blue b.blue)

let print_clr a = Printf.printf "\ncolor is [r: %4.2f g: %4.2f b: %4.2f]" a.red a.green a.blue

let scale_clr x = 
  let scaled = 255. *. x in
  if scaled > 255. then 255
  else if scaled < 0. then 0
  else int_of_float scaled

type pixel =
  | Color of color
  | Blank

let pix_to_col = function
  | Color c -> c
  | Blank -> make_color 0. 0. 0.

type canvas = {
  height: int;
  width: int;
  grid: pixel array array
}

let pixel_to_string_P3 pixel = 
  match pixel with
  | Blank -> Printf.sprintf "%-3d %-3d %-3d" 0 0 0
  | Color c -> Printf.sprintf "%-3d %-3d %-3d" (scale_clr c.red) (scale_clr c.green) (scale_clr c.blue)

let make_canvas ~w ~h = {width=w; height=h; grid= Array.make_matrix w h Blank}

let write_canvas_P3 ~oc ~can =
  (* following line writes initial text (format, dims, color scale) to file*)
  Printf.fprintf oc "P3\n%d %d\n255\n" can.width can.height;
  for y = 0 to pred can.height do
    for x = 0 to pred can.width do 
      Printf.fprintf oc "%-12s  " (pixel_to_string_P3 can.grid.(x).(y));
    done;
    Printf.fprintf oc "\n,";
  done;
;; 

let write_canvas_P6 ~oc ~can =
  Printf.fprintf oc "P6 %d %d 255\n" can.width can.height;
  for y = 0 to pred can.height do
    for x = 0 to pred can.width do
      match can.grid.(x).(y) with
        | Blank -> begin
          output_char oc (char_of_int 0);
          output_char oc (char_of_int 0);
          output_char oc (char_of_int 0);
        end
        | Color c -> begin
          output_char oc (char_of_int (scale_clr c.red));
          output_char oc (char_of_int (scale_clr c.green));
          output_char oc (char_of_int (scale_clr c.blue));
        end
    done;
  done;
  output_char oc '\n';
  flush oc;
;;

  
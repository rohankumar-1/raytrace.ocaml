

let equal_float a b = (abs_float (a -. b)) < 0.0001

let scale x = 
  let scaled = 255. *. x in
  if scaled > 255. then 255
  else if scaled < 0. then 0
  else int_of_float scaled
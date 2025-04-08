open Graphics
open Transform
open Draw
open Data
open Shapes


let () = 

  let floor = new plane in 
  floor#set_material (make_material ~pat:(make_plain (make_color 1.0 0.9 0.9)) ~reflect:0.2 ());

  let lwall = new plane in 
  let tf = chain_transforms [(scale 0.2 0.2 0.2) ; rotate_y (Float.pi /. 2.)] in 
  lwall#set_material (make_material ~pat:(make_stripe (_GREEN) (_RED) tf) ());
  lwall#set_transform (chain_transforms [(translate 0. 0. 5.) ; (rotate_y (Float.pi /. 4.)) ; (rotate_x (Float.pi /. 2.)) ]);

  let rwall = new plane in 
  rwall#set_material (make_material ~pat:(make_plain (_BLUE)) ());
  rwall#set_transform (chain_transforms [(translate 0. 0. 5.) ; (rotate_y (-.Float.pi /. 4.)) ; (rotate_x (Float.pi /. 2.)) ]);


  let middle = new sphere in 
  let mid_pat = make_stripe _WHITE _GREEN (chain_transforms [(scale 0.2 0.2 0.2);(rotate_z (Float.pi /. 4.))]) in 
  middle#set_transform (translate (-0.5) 1. 0.5);
  middle#set_material (make_material ~pat:mid_pat ~di:0.7 ~sp:0.3 ());

  let right = new sphere in 
  right#set_transform (chain_transforms [
    translate 1.5 0.5 (-0.5) ; 
    scale 0.5 0.5 0.5;
    ]);
  right#set_material (make_material ~pat:(Plain(make_color 0.5 1. 0.1)) ~di:0.7 ~sp:0.3 ());

  let left = new sphere in 
  left#set_transform (chain_transforms [
    translate (-1.5) 0.33 (-0.75) ; 
    scale 0.33 0.33 0.33;
    ]);
  left#set_material (make_material ~pat:(Plain(make_color 1. 0.8 0.1)) ~di:0.7 ~sp:0.3 ());

  let world = {
    light = make_light (make_color 1. 1. 1.) (point (-10.) 10. (-10.)); 
    objects = [
      floor;
      rwall;
      lwall;
      left;
      right;
      middle;
    ];
  } in 

  let cam = make_camera 800 400 (Float.pi /. 3.) in 
  cam.tf := view_transform (point 0. 1.5 (-5.)) (point 0. 1. 0.) (vector 0. 1. 0.);

  let img = render cam world ~reflect_recurse:2 () in 
  let oc = open_out "output/scene_w_reflect.ppm" in 
  write_canvas_P6 ~oc:oc ~can:img;


open Labyrinthe

let	tile_size = 50

let get_pos_x_1 case = 
  match Case.get_list_state case with
  | [Case.Close; Case.Open; Case.Open; Case.Open]	-> 0
  | [Case.Open; Case.Open; Case.Open; Case.Close]	-> 50
  | [Case.Open; Case.Open; Case.Close; Case.Open]	-> 100
  | [Case.Open; Case.Close; Case.Open; Case.Open]	-> 150
  | _							-> 0

let get_pos_x_2 case = 
  match Case.get_list_state case with
  | [Case.Close; Case.Open; Case.Open; Case.Close]	-> 0
  | [Case.Close; Case.Close; Case.Open; Case.Open]	-> 50
  | [Case.Open; Case.Open; Case.Close; Case.Close]	-> 100
  | [Case.Open; Case.Close; Case.Close; Case.Open]	-> 150
  | [Case.Open; Case.Close; Case.Open; Case.Close]	-> 200
  | [Case.Close; Case.Open; Case.Close; Case.Open]	-> 250
  | _							-> 0

let get_pos_x_3 case = 
  match Case.get_list_state case with
  | [Case.Close; Case.Close; Case.Open; Case.Close]	-> 0
  | [Case.Close; Case.Close; Case.Close; Case.Open]	-> 50
  | [Case.Open; Case.Close; Case.Close; Case.Close]	-> 100
  | [Case.Close; Case.Open; Case.Close; Case.Close]	-> 150
  | _							-> 0
let get_pos_test case = 
  match Case.get_list_state case with
  | [Case.Close; Case.Open; Case.Open; Case.Open]	-> (0, 50)
  | [Case.Open; Case.Open; Case.Open; Case.Close]	-> (50, 50)
  | [Case.Open; Case.Open; Case.Close; Case.Open]	-> (100, 50)
  | [Case.Open; Case.Close; Case.Open; Case.Open]	-> (150, 50)
  | [Case.Close; Case.Open; Case.Open; Case.Close]	-> (0, 100)
  | [Case.Close; Case.Close; Case.Open; Case.Open]	-> (50, 100)
  | [Case.Open; Case.Open; Case.Close; Case.Close]	-> (100, 100)
  | [Case.Open; Case.Close; Case.Close; Case.Open]	-> (150, 100)
  | [Case.Open; Case.Close; Case.Open; Case.Close]	-> (200, 100)
  | [Case.Close; Case.Open; Case.Close; Case.Open]	-> (250, 100)
  | [Case.Close; Case.Close; Case.Open; Case.Close]	-> (0, 150)
  | [Case.Close; Case.Close; Case.Close; Case.Open]	-> (50, 150)
  | [Case.Open; Case.Close; Case.Close; Case.Close]	-> (100, 150)
  | [Case.Close; Case.Open; Case.Close; Case.Close]	-> (150, 150)
  | _							-> (0, 0)

(*let print_l case = *)
  

let	get_pos case = 
  match Case.nb_close case with
  | 0	-> (0, 0)
  | 1	-> (get_pos_x_1 case, 50)
  | 2	-> (get_pos_x_2 case, 100)
  | 3	-> (get_pos_x_3 case, 150)
  | 4	-> (0, 200)
  | _	-> (0, 0)
  


let	render sx sy =
  let map = create sx sy 4 in
  let screen = Sdlvideo.set_video_mode (sx * tile_size) (sy * tile_size) [] in
  let texture =  Sdlloader.load_image "tiles4.png" in
  let rec loop_in x y =
    if x = sx
    then 
      if y = sy - 1
      then map
      else loop_in 0 (y + 1)
    else
      let new_pos = Sdlvideo.rect (x * 50) (y * 50) 0 0 in
      let pos_on_tileset = (get_pos_test (get_case map x y)) in
      let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) 50 50 in
      Sdlvideo.blit_surface ~dst:screen ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ();
      print_endline ("x: " ^ (string_of_int x) ^ " y: " ^ (string_of_int y) ^ " " ^ (string_of_int (Case.nb_close (get_case map x y))));
      loop_in (x + 1) y
  in loop_in 0 0
  
let	main () =
  Sdl.init [`VIDEO];
  print_lab (render 4 4);
  print_endline "plop";
  Sdlvideo.flip (Sdlvideo.get_video_surface ());
  Sdltimer.delay 8000;
  Sdl.quit ()

  
let _ = main ()


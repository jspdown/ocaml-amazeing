open Sdlevent
open Sdlkey
open Labyrinthe

let     tile_size = 50
let	time = 0
let	music_filename = "mortalCombat.wav"

let image = Sdlloader.load_image "troll.png"
let win_icon = Sdlloader.load_image "battered-axe.png"
let texture =  Sdlloader.load_image "t.png"
let objective = Sdlloader.load_image "ham.png"

let get_pos case =
  match Case.get_list_state case with
    | [Case.Close; Case.Open; Case.Open; Case.Open]       -> (0, 50)
    | [Case.Open; Case.Open; Case.Open; Case.Close]       -> (50, 50)
    | [Case.Open; Case.Open; Case.Close; Case.Open]       -> (100, 50)
    | [Case.Open; Case.Close; Case.Open; Case.Open]       -> (150, 50)
    | [Case.Close; Case.Open; Case.Open; Case.Close]      -> (0, 100)
    | [Case.Close; Case.Close; Case.Open; Case.Open]      -> (50, 100)
    | [Case.Open; Case.Open; Case.Close; Case.Close]      -> (100, 100)
    | [Case.Open; Case.Close; Case.Close; Case.Open]      -> (150, 100)
    | [Case.Open; Case.Close; Case.Open; Case.Close]      -> (200, 100)
    | [Case.Close; Case.Open; Case.Close; Case.Open]      -> (250, 100)
    | [Case.Close; Case.Close; Case.Open; Case.Close]     -> (0, 150)
    | [Case.Close; Case.Close; Case.Close; Case.Open]     -> (50, 150)
    | [Case.Open; Case.Close; Case.Close; Case.Close]     -> (100, 150)
    | [Case.Close; Case.Open; Case.Close; Case.Close]     -> (150, 150)
    | _                                                   -> (0, 0)

let	check_collision x case =
  match (x, Case.get_list_state case) with
    | (0, [Case.Close; _ ; _ ; _])	-> 1
    | (1, [ _ ; Case.Close; _ ; _])	-> 1
    | (2, [ _ ; _ ; Case.Close; _])	-> 1
    | (3, [ _ ; _ ; _ ; Case.Close])	-> 1
    | _					-> 0

let     put_map_back x y screen map =
  let new_pos = Sdlvideo.rect (x * tile_size) (y * tile_size) 0 0 in
  let pos_on_tileset = (get_pos (get_case map x y)) in
  let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) tile_size tile_size in
  Sdlvideo.blit_surface  ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ~dst:screen ();
  Sdlvideo.flip screen

let	move x y screen =
  let position_of_image = Sdlvideo.rect (x * tile_size) (y * tile_size) tile_size tile_size in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlvideo.flip screen

let	print_objective x y screen =
  let position_of_image = Sdlvideo.rect (x * tile_size) (y * tile_size) tile_size tile_size in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:objective ~dst:screen ();
  Sdlvideo.flip screen

let	increment i =
  i + 1

let	decrement i =
  i - 1

let     render_solve screen map start finish =
  let texture =  Sdlloader.load_image "s_tile.png" in
  let s = solve map start finish in
  let rec process_solve screen l =
    match l with
      | []        -> map
      | h::t      -> 
	let blt_pos = Sdlvideo.rect (snd h * tile_size) (fst h * tile_size) 0 0 in
	let rect_pos = Sdlvideo.rect (0) (0) tile_size tile_size in
	begin
          Sdlvideo.blit_surface ~dst:screen ~dst_rect:blt_pos ~src:texture ~src_rect:rect_pos ();
          process_solve screen (t)
	end
  in process_solve screen s

let render_clear sx sy screen map =
  let rec loop_in_clear x y =
    if x = sx
    then
      if y = sy - 1
      then map
      else loop_in_clear 0 (y + 1)
    else
      let new_pos = Sdlvideo.rect (x * tile_size) (y * tile_size) 0 0 in
      let pos_on_tileset = (get_pos (get_case map x y)) in
      let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) tile_size tile_size in
      Sdlvideo.blit_surface ~dst:screen ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ();
      loop_in_clear (x + 1) y
  in loop_in_clear 0 0

let render sx sy screen =
  let map = create sx sy 4 in
  let rec loop_in x y =
    if x = sx
    then
      if y = sy - 1
      then map
      else 
	loop_in 0 (y + 1)
    else
      let new_pos = Sdlvideo.rect (x * tile_size) (y * tile_size) 0 0 in
      let pos_on_tileset = (get_pos (get_case map x y)) in
      let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) tile_size tile_size in
      Sdlvideo.blit_surface ~dst:screen ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ();
      loop_in (x + 1) y
  in loop_in 0 0

let	rec wait_for_escape x y screen l h map to_solve =
  match wait_event () with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
      print_endline "Goodbye! See you next time."
    | QUIT ->
      print_endline "Goodbye! See you next time."
    | KEYDOWN {keysym=KEY_UP} ->
      if y > 0 && check_collision 0 (get_case map x y) == 0
      then
	let y2 = decrement y in
	put_map_back x y screen map;
	move x y2 screen;
	wait_for_escape x y2 screen l h map to_solve
      else
	wait_for_escape x y screen l h map to_solve
    | KEYDOWN {keysym=KEY_DOWN} ->
      if y < (h - 1) && check_collision 2 (get_case map x y) == 0
      then
	let y2 = increment y in
	put_map_back x y screen map;
	move x y2 screen;
	wait_for_escape x y2 screen l h map to_solve
      else
	wait_for_escape x y screen l h map to_solve
    | KEYDOWN {keysym=KEY_LEFT} ->
      if x > 0 && check_collision 3 (get_case map x y) == 0
      then
	let x2 = decrement x in
	put_map_back x y screen map;
	move x2 y screen;
	wait_for_escape x2 y screen l h map to_solve
      else
	wait_for_escape x y screen l h map to_solve
    | KEYDOWN {keysym=KEY_RIGHT} ->
      if x < (l - 1) && check_collision 1 (get_case map x y) == 0
      then
	let x2 = increment x in
	put_map_back x y screen map;
	move x2 y screen;
	wait_for_escape x2 y screen l h map to_solve
      else
	wait_for_escape x y screen l h map to_solve
    | KEYDOWN {keysym=KEY_SPACE} ->
      if  to_solve
      then
        begin
          render_clear l h screen map;
          Sdlvideo.flip screen
        end
      else
        ();
      wait_for_escape x y screen l h map (not to_solve)
    | KEYDOWN {keysym=KEY_r} ->
      wait_for_escape x y screen l h (render l h screen) to_solve;
    | event ->
      if to_solve
      then
	begin
	  render_clear l h screen map;
	  render_solve screen map (x,y) (9,9); (*TODO*)
	  Sdlvideo.flip screen
	end
      else
	();
      move x y screen;
      print_objective 9 9 screen; (*TODO*)
      if x = 9 && y = 9 (*TODO*)
      then
	print_endline "Game Over! Well played!"
      else
	wait_for_escape x y screen l h map to_solve

let	run x y =
  let screen = Sdlvideo.set_video_mode (x * tile_size) (y * tile_size) [`DOUBLEBUF] in
  let music = Sdlmixer.load_music music_filename in
  let position_of_image = Sdlvideo.rect 0 0 tile_size tile_size in (*TODO pos joueur*)
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlwm.set_caption "A-Maze-Ing" "battered-axe.png";
  Sdlwm.set_icon win_icon;
  Sdlvideo.flip screen;
  Sdlmixer.fadein_music music 1.0;
  begin
    wait_for_escape 0 0 screen x y (render x y screen) false; (*TODO*)
    Sdlvideo.flip screen;
  end

let	main () =
  Sdl.init [`VIDEO; `AUDIO; `TIMER];
  at_exit Sdl.quit;
  Sdlmixer.open_audio ();
  at_exit Sdlmixer.close_audio;
  run 10 10 (*TODO*)

let _ = main ()

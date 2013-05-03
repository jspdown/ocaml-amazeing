open Sdlevent
open Sdlkey
open Labyrinthe

let     tile_size = 50
let	image_filename = "battered-axe.png"
let	blank = "black.png"
let	sprite_sheet = "tiles4.png"
let	music_filename = "mortalCombat.wav"

let image = Sdlloader.load_image image_filename
let texture =  Sdlloader.load_image sprite_sheet
let empty = Sdlloader.load_image blank


let get_pos_x_1 case =
  match Case.get_list_state case with
    | [Case.Close; Case.Open; Case.Open; Case.Open]       -> 0
    | [Case.Open; Case.Open; Case.Open; Case.Close]       -> 50
    | [Case.Open; Case.Open; Case.Close; Case.Open]       -> 100
    | [Case.Open; Case.Close; Case.Open; Case.Open]       -> 150
    | _                                                   -> 0

let get_pos_x_2 case =
  match Case.get_list_state case with
    | [Case.Close; Case.Open; Case.Open; Case.Close]      -> 0
    | [Case.Close; Case.Close; Case.Open; Case.Open]      -> 50
    | [Case.Open; Case.Open; Case.Close; Case.Close]      -> 100
    | [Case.Open; Case.Close; Case.Close; Case.Open]      -> 150
    | [Case.Open; Case.Close; Case.Open; Case.Close]      -> 200
    | [Case.Close; Case.Open; Case.Close; Case.Open]      -> 250
    | _                                                   -> 0

let get_pos_x_3 case =
  match Case.get_list_state case with
    | [Case.Close; Case.Close; Case.Open; Case.Close]     -> 0
    | [Case.Close; Case.Close; Case.Close; Case.Open]     -> 50
    | [Case.Open; Case.Close; Case.Close; Case.Close]     -> 100
    | [Case.Close; Case.Open; Case.Close; Case.Close]     -> 150
    | _                                                   -> 0
let get_pos_test case =
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

let     get_pos case =
  match Case.nb_close case with
    | 0   -> (0, 0)
    | 1   -> (get_pos_x_1 case, 50)
    | 2   -> (get_pos_x_2 case, 100)
    | 3   -> (get_pos_x_3 case, 150)
    | 4   -> (0, 200)
    | _   -> (0, 0)

let     arg_get_size =
  match Array.length Sys.argv with
    | 3   -> (int_of_string Sys.argv.(1), int_of_string Sys.argv.(2))
    | _   -> (10, 10)

let	move x y screen image =
  let position_of_image = Sdlvideo.rect x y 50 50 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlvideo.flip screen

let     put_map_back x y screen map =
  let new_pos = Sdlvideo.rect (x * 50) (y * 50) 0 0 in
  let pos_on_tileset = (get_pos (get_case map x y)) in
  let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) 50 50 in
  Sdlvideo.blit_surface  ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ~dst:screen ();
  Sdlvideo.flip screen

let	increment i =
  i + 50

let	decrement i =
  i - 50

let	rec wait_for_escape x y s i e l h map =
  match wait_event () with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
      print_endline "Goodbye! See you next time."
    | QUIT ->
      print_endline "Goodbye! See you next time."
    | KEYDOWN {keysym=KEY_UP} ->
      if y > 0
      then
	let y2 = decrement y in 
	put_map_back (x / 50) (y / 50) s map;
	move x y2 s i;
	wait_for_escape x y2 s i e l h map
      else
	wait_for_escape x y s i e l h map
    | KEYDOWN {keysym=KEY_DOWN} ->
      if y < ((h - 1) * tile_size)
      then
	let y2 = increment y in
	put_map_back (x/50) (y/50) s map;
	move x y2 s i;
	wait_for_escape x y2 s i e l h map
      else
	wait_for_escape x y s i e l h map
    | KEYDOWN {keysym=KEY_LEFT} ->
      if x > 0
      then
	let x2 = decrement x in
	put_map_back (x/50) (y/50) s map;
	move x2 y s i;
	wait_for_escape x2 y s i e l h map
      else
	wait_for_escape x y s i e l h map
    | KEYDOWN {keysym=KEY_RIGHT} ->
      if x < ((l - 1) * tile_size)
      then
	let x2 = increment x in
	put_map_back (x/50) (y/50) s map;
	move x2 y s i;
	wait_for_escape x2 y s i e l h map
      else
	wait_for_escape x y s i e l h map
    | KEYDOWN {keysym=KEY_SPACE} ->
      print_endline "test";
	wait_for_escape x y s i e l h map
    | event ->
      wait_for_escape x y s i e l h map

let     render sx sy screen =
  let map = create sx sy 4 in
  let rec loop_in x y =
    if x = 10
    then
      if y = 9
      then map
      else loop_in 0 (y + 1)
    else
      let new_pos = Sdlvideo.rect (x * 50) (y * 50) 0 0 in
      let pos_on_tileset = (get_pos (get_case map x y)) in
      let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) 50 50 in
      Sdlvideo.blit_surface ~dst:screen ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ();
      Sdlvideo.flip screen;
      loop_in (x + 1) y
  in loop_in 0 0

let	run (x, y) =
  let screen = Sdlvideo.set_video_mode (x * tile_size) (y * tile_size) [`DOUBLEBUF] in
  let music = Sdlmixer.load_music music_filename in
  let map = render 10 10 screen in
  let position_of_image = Sdlvideo.rect 0 0 50 50 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlwm.set_caption "A-Maze-Ing" "battered-axe.png";
  Sdlwm.set_icon image;
  Sdlvideo.flip screen;
  Sdlmixer.fadein_music music 1.0;
  Sdltimer.delay 1000;
  wait_for_escape 0 0 screen image empty x y map

let	main () =
  Sdl.init [`VIDEO; `AUDIO];
  at_exit Sdl.quit;
  Sdlmixer.open_audio ();
  (*at_exit Sdlmixer.fadeout_music 2.0;
  at_exit Sdltimer.delay 1000;
  at_exit Sdlmixer.halt_music ();
  at_exit Sdlmixer.free_music music;*)
  at_exit Sdlmixer.close_audio;
  run arg_get_size

let _ = main ()

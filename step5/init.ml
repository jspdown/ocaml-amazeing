open Sdlevent
open Sdlkey

let     tile_size = 50
let	image_filename = "battered-axe.png"
let	image_filenam = "black.png"
let	music_filename = "mortalCombat.wav"

let     arg_get_size =
  match Array.length Sys.argv with
    | 3   -> (int_of_string Sys.argv.(1), int_of_string Sys.argv.(2))
    | _   -> (10, 10)

let	move x y screen image =
  let position_of_image = Sdlvideo.rect x y 50 50 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlvideo.flip screen

let	increment i =
  i + 50

let	decrement i =
  i - 50

let	rec wait_for_escape x y s i e l h =
  match wait_event () with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
      print_endline "Goodbye! See you next time."
    | QUIT ->
      print_endline "Goodbye! See you next time."
    | KEYDOWN {keysym=KEY_UP} ->
      print_endline ("UP");
      if y > 0
      then
	let y2 = decrement y in 
	move x y s e;
	move x y2 s i;
	wait_for_escape x y2 s i e l h
      else
	wait_for_escape x y s i e l h
    | KEYDOWN {keysym=KEY_DOWN} ->
      print_endline ("DOWN");
      if y < ((h - 1) * tile_size)
      then
	let y2 = increment y in
	move x y s e;
	move x y2 s i;
	wait_for_escape x y2 s i e l h
      else
	wait_for_escape x y s i e l h
    | KEYDOWN {keysym=KEY_LEFT} ->
      print_endline ("LEFT");
      if x > 0
      then
	let x2 = decrement x in
	move x y s e;
	move x2 y s i;
	wait_for_escape x2 y s i e l h
      else
	wait_for_escape x y s i e l h
    | KEYDOWN {keysym=KEY_RIGHT} ->
      print_endline ("RIGHT");
      if x < ((l - 1) * tile_size)
      then
	let x2 = increment x in
	move x y s e;
	move x2 y s i;
	wait_for_escape x2 y s i e l h
      else
	wait_for_escape x y s i e l h
    | event ->
      wait_for_escape x y s i e l h

let	run (x, y) =
  let screen = Sdlvideo.set_video_mode (x * tile_size) (y * tile_size) [`DOUBLEBUF] in
  let image = Sdlloader.load_image image_filename in
  let empty = Sdlloader.load_image image_filenam in
  let music = Sdlmixer.load_music music_filename in
  let position_of_image = Sdlvideo.rect 0 0 50 50 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlwm.set_caption "A-Maze-Ing" "battered-axe.png";
  Sdlwm.set_icon image;
  Sdlvideo.flip screen;
  Sdlmixer.fadein_music music 1.0;
  Sdltimer.delay 1000;
  Sdlmixer.fadeout_music 2.0;
  Sdltimer.delay 1000;
  Sdlmixer.halt_music ();
  Sdlmixer.free_music music;
  wait_for_escape 0 0 screen image empty x y

let	main () =
  Sdl.init [`VIDEO; `AUDIO];
  at_exit Sdl.quit;
  Sdlmixer.open_audio ();
  at_exit Sdlmixer.close_audio;
  run arg_get_size

let _ = main ()

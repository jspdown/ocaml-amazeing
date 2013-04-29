
let	tile_size = 50
let	ressource_file = "tileset.bmp"

let	arg_get_size = 
  match Array.length Sys.argv with
  | 3	-> (int_of_string Sys.argv.(1), int_of_string Sys.argv.(2))
  | _	-> (10, 10)


let	init_sdl_window (x, y) =
  Sdl.init [`VIDEO];
  Sdlvideo.set_video_mode (x * tile_size) (y * tile_size) []

let	main () =
  init_sdl_window arg_get_size;
  Sdltimer.delay 2000;
  Sdl.quit ()

  
let _ = main ()

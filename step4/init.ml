open Sdlevent
open Sdlkey
open Labyrinthe

let	tile_size = 50

let get_pos case =
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

let	render_solve screen map sx sy =
  let texture =  Sdlloader.load_image "s_tile.png" in
  let s = solve map (0, 0) (sx - 1, sy -1) in
  let rec process_solve screen l =
    match l with
    | []	-> map
    | h::t	->
      let blt_pos = Sdlvideo.rect (snd h * tile_size) (fst h * tile_size) 0 0 in
      let rect_pos = Sdlvideo.rect (0) (0) tile_size tile_size in
      begin
	Sdlvideo.blit_surface ~dst:screen ~dst_rect:blt_pos ~src:texture ~src_rect:rect_pos ();
	process_solve screen (t)
      end
  in process_solve screen s

let render sx sy =
  let map = create sx sy 4 in
  let screen = Sdlvideo.set_video_mode (sx * tile_size) (sy * tile_size) [] in
  let texture =  Sdlloader.load_image "t.png" in
  let rec loop_in x y =
    if x = sx
    then
      if y = sy - 1
      then render_solve screen map sx sy
      else loop_in 0 (y + 1)
    else
      let new_pos = Sdlvideo.rect (x * tile_size) (y * tile_size) 0 0 in
      let pos_on_tileset = (get_pos (get_case map x y)) in
      let rect_pos = Sdlvideo.rect (fst pos_on_tileset) (snd pos_on_tileset) tile_size tile_size in
      Sdlvideo.blit_surface ~dst:screen ~dst_rect:new_pos ~src:texture ~src_rect:rect_pos ();
      loop_in (x + 1) y
  in loop_in 0 0

let rec main_loop () =
  match wait_event () with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
    print_endline "Goodbye! See you next time."
  | QUIT ->
    print_endline "Goodbye! See you next time."
  | _    -> main_loop ()

let	main height width =
  let screen = Sdlvideo.set_video_mode (width * tile_size) (height * tile_size) [`DOUBLEBUF] in
  Sdl.init [`VIDEO];
  render_solve screen (render height width) height width;
  Sdlvideo.flip (Sdlvideo.get_video_surface ());
  main_loop ();
  Sdl.quit ()

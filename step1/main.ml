let usage = "[Usage]:\t./step height width"

let height = ref (-1)
let width = ref (-1)

let main () =
  begin
    Arg.parse [] (fun i -> if !height < 0 then height := (int_of_string i) else width := (int_of_string i)) usage;
    Labyrinthe.print_lab (Labyrinthe.create !height !width 4);
  end

let _ = main ()

let usage = "[Usage]:\t./step height width\n"

let height = ref (-1)
let width = ref (-1)

let main () =
  begin
    Arg.parse [] (fun i -> if !height < 0 then height := (int_of_string i) else width := (int_of_string i)) usage;
    if !height <= 1 || !width <= 1
    then
      begin
	print_string "height and width need to be superior than 1\n";
	print_string usage;
      end
    else
      Init.main !height !width
  end

let _ = main ()

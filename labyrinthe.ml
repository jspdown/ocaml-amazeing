module Case : sig
  type state = Close | Open
  type case

  val length	 : case -> int
  val get	 : case -> int -> case
  val get_open	 : case -> int -> case
  val get_close	 : case -> int -> case

  val set	 : case -> int -> case -> state -> unit
  val equal	 : case -> case -> bool
  val inter	 : case -> case -> bool
  val inter_idx	 : case -> case -> int
  val open_door  : case -> int -> unit
  val close_door : case -> int -> unit
  val create	 : int -> case

  val change_state_door : state -> case -> int -> unit

  exception Invalid_case
  exception Not_connect_case

end = struct

  type state = Close | Open

  type case =
  | Case of (case ref * state) array ref
  | None

  exception Invalid_case
  exception Not_connect_case

  (* Function implementation *)

  let length case = match case with
    | Case(a) -> Array.length !a
    | None    -> 0

  let get_idx case i = match case with
    | Case(a) -> Array.get !a i
    | None    -> raise Invalid_case

  let get case i = !(fst (get_idx case i))

  let rec get_state state case i = match (get_idx case i) with
    | Case (c, status) ->
      if status = state
      then
	i
      else
	get_state case (i + 1)
    | None -> get_state case (i + 1)

  let set case i item state = match case with
    | Case(a) -> Array.set !a i (ref item, state)
    | None    -> raise Invalid_case

  let equal case1 case2 = match case1, case2 with
    | Case(a), Case(b)  -> a == b
    | None, None	-> true
    | _, _		-> false

  let inter_idx case1 case2 =
    let rec inter_rec case1 case2 i =
      if (length case1) <= i
      then
  	-1
      else
  	if equal (get case1 i) case2
  	then
  	  i
  	else
  	  inter_rec case1 case2 (i + 1)
    in
    inter_rec case1 case2 0

  let inter case1 case2 = (inter_idx case1 case2) >= 0

  let change_one_side_state state case i = match case with
    | None -> raise Invalid_case
    | _    -> set case i (get case i) state

  let change_state_door state case i =
    let linked = get case i in
    let link_idx = inter_idx linked case in
    begin
      if link_idx < 0
      then
	raise Not_connect_case
      else
	change_one_side_state state linked link_idx;
      change_one_side_state state case i;
    end

  let open_door = change_state_door Open

  let close_door = change_state_door Close

  let create i =
    Case (ref (Array.make i (ref None, Close)))
end

type labyrinthe = case array

(* let create height width size = *)
(*   let tab = Array.init (height * width) (fun a -> (a, Case.create size)) in *)
(*   let open_rand_door case = Case.open_door case (Rand.int (Case.length case)) in *)
(*   tab *)

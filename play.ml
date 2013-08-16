open Array
open Color
open Command
open Theory

type board = int64 * int64 * int * int

let init_board () =
  (34628173824L, 68853694464L, 2, 2)

let get_color (board_b, board_w, count_b, count_w) (i, j) =
  (* Assert 0 <= i, j <= 7 *)
  let n = i + j * 8 in
  let n_bit = Int64.shift_right_logical Int64.min_int n in
  if Int64.logand board_b n_bit <> Int64.zero then
    black
  else if Int64.logand board_w n_bit <> Int64.zero then
    white
  else
    none

let set_color (board_b, board_w, count_b, count_w) (i, j) color =
  (* Assert 0 <= i, j <= 7 *)
  let n = i + j * 8 in
  let n_bit = Int64.shift_right_logical Int64.min_int n in
  let n_bit' = Int64.lognot n_bit in
  let previous = get_color (board_b, board_w, count_b, count_w) (i, j) in
  let count_b = if previous <> black && color = black then count_b + 1
    else if previous = black && color <> black then count_b - 1
    else count_b
  in
  let count_w = if previous <> white && color = white then count_w + 1
    else if previous = white && color <> white then count_w - 1
    else count_w
  in
  if color = black then
    (Int64.logor board_b n_bit, Int64.logand board_w n_bit', count_b, count_w)
  else if color = white then
    (Int64.logand board_b n_bit', Int64.logor board_w n_bit, count_b, count_w)
  else
    (Int64.logand board_b n_bit', Int64.logand board_w n_bit', count_b, count_w)

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let valid_coordinates (i, j) = 0 <= i && i <= 7 && 0 <= j && j <= 7

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if valid_coordinates (i, j) && get_color board (i, j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if valid_coordinates (i, j) && get_color board (i, j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if valid_coordinates (i, j) && get_color board (i, j) = color then
      r
    else
      [] in
  f (di,dj) (i,j) []

let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
  List.concat bs

let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with
    [] -> false
  | _  -> true

let is_valid_move board color (i,j) =
  (get_color board (i, j) = none) && is_effective board color (i,j)


let doMove board com color =
  match com with
    GiveUp  -> board
  | Pass    -> board
  | Mv (i,j) ->
    let ms = (i, j) :: flippable_indices board color (i,j) in
    List.fold_left (fun b p -> set_color b p color) board ms

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color =
  let ls = [0;1;2;3;4;5;6;7] in
  List.filter (is_valid_move board color)
    (mix ls ls)


let print_human board color valid_moves =
  let counter = ref 1 in
  let moves_sorted = ref [] in
  (* print_string "\x1b[2J"; *)
  for j = 0 to 7 do 
    for i = 0 to 7 do
      if List.mem (i, j) valid_moves then
	( if color = white then Printf.printf "\x1b[37;46m%2x\x1b[m" !counter
	  else                  Printf.printf "\x1b[30;46m%2x\x1b[m" !counter ;
	  moves_sorted := (i, j) :: !moves_sorted;
	  counter := !counter + 1 )
      else
	let c = get_color board (i, j) in
	if c = white then print_string "\x1b[01;37;42m● \x1b[m"
	else if c = black then print_string "\x1b[01;30;42m● \x1b[m"
	else print_string "\x1b[01;33;42m+ \x1b[m"
    done;
    print_endline ""
  done;
  !moves_sorted


let count (board_b, board_w, count_b, count_w) color =
  if color = black then count_b
  else if color = white then count_w
  else 64 - count_b - count_w

let phase (board_b, board_w, count_b, count_w) = count_b + count_w

let search_alphabeta board color depth evaluator =
  let ocolor = opposite_color color in
  let rec search_me board endflag alpha beta depth =
    let ms = valid_moves board color in
    if ms = [] || depth = 0 then
      if endflag then
	evaluator board color
      else
	search_enemy board true alpha beta (depth - 1)
    else
      let rec search ms alpha =
	match ms with
	  [] -> alpha
	| (i, j)::ms -> let nextboard = doMove board (Mv (i, j)) color in
			let alpha = max alpha (search_enemy nextboard false alpha beta (depth - 1)) in
			if alpha >= beta then beta else search ms alpha
      in
      search ms alpha
  and search_enemy board endflag alpha beta depth =
    let ms = valid_moves board ocolor in
    if ms = [] || depth = 0 then
      if endflag then
	evaluator board color
      else
	search_me board true alpha beta (depth - 1)
    else
      let rec search ms beta =
	match ms with
	  [] -> beta
	| (i, j)::ms -> let nextboard = doMove board (Mv (i, j)) ocolor in
			let beta = min beta (search_me nextboard false alpha beta (depth - 1)) in
			if alpha >= beta then alpha else search ms beta
      in
      search ms beta
  in
  let rec search_firstmove board ms max_s max_hand depth =
    match ms with
      [] -> (max_s, max_hand)
    | (i, j)::ms -> let nextboard = doMove board (Mv (i, j)) color in
		    let try_s = search_enemy nextboard false (-999) 999 (depth - 1) in
		    if max_s <= try_s then
		      search_firstmove board ms try_s (Mv (i, j)) depth
		    else
		      search_firstmove board ms max_s max_hand depth
  in search_firstmove board (valid_moves board color) (-1) Pass depth

let search_endstage board color =
  let vm = valid_moves board color in
  let _ = print_human board color vm in
  print_endline "[END STAGE] Computing all possible moves...";
  let (max_s, max_hand) = search_alphabeta board color (-1) count
  in
  print_string "Expectation: ";
  print_int max_s;
  print_endline "";
  max_hand


let search_priority board color =
  let priority_list =
    [
      (0,0);(0,7);(7,0);(7,7);
      (0,2);(2,0);(0,5);(5,0);(2,7);(7,2);(5,7);(7,5);
      (2,2);(2,5);(5,2);(5,5);
      (0,3);(0,4);(3,0);(4,0);(3,7);(4,7);(7,3);(7,4);
      (2,3);(2,4);(3,2);(4,2);(3,4);(3,5);(4,3);(5,3);
      (1,2);(1,3);(1,4);(1,5);(2,1);(3,1);(4,1);(5,1);(2,6);(3,6);(4,6);(5,6);(6,2);(6,3);(6,4);(6,5);
      (0,1);(1,0);(0,6);(1,7);(6,0);(7,1);(6,7);(7,6);
      (1,1);(1,6);(6,1);(6,6)
    ] in
  let rec search move =
    match move with (i, j)::ms ->
      if is_valid_move board color (i, j) then Mv (i, j)
      else search ms
    | [] -> Pass
  in
  search priority_list

let search_theory hist board color =
  let len = String.length hist in
  let rec search theory =
    match theory with
      [] ->
	print_endline "[EARLY STAGE] No theory found";
	search_priority board color
    | t::ts ->
      if theory_enroute t hist len then
	( print_endline "[EARLY STAGE] I know theory!";
	  theory_decode t.[len] )
      else search ts
  in search theory_opening

let play history_code board color =
  let phase = phase board in
  print_string "Phase: ";
  print_int phase;
  print_endline "";
  if phase = 4 then
    Mv (5, 4)
  else if phase > 50 then
    let time_start = Unix.gettimeofday () in
    let result = search_endstage board color in
    Printf.printf "%f seconds.\n" (Unix.gettimeofday () -. time_start);
    result
  else
    search_theory history_code board color

let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j = 0 to 7 do
    print_int (j + 1); print_string "|";
    for i = 0 to 7 do
      print_color (get_color board (i, j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
  if bc > wc then
    print_endline "*Black wins!*"
  else if bc < wc then
    print_endline "*White wins!*"
  else
    print_endline "*Even*";
  print_string "Black: "; print_endline (string_of_int bc);
  print_string "White: "; print_endline (string_of_int wc);
  print_board board

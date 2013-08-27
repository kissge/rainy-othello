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

let all_pos =
  let ls = [0;1;2;3;4;5;6;7] in
  mix ls ls

let valid_moves board color = List.filter (is_valid_move board color) all_pos


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

let search_alphabeta board color depth evaluator cut =
  let ocolor = opposite_color color in
  let rec search_me board endflag alpha beta depth =
    if depth = 0 then
      evaluator color board color
    else
      let ms = valid_moves board color in
      if ms = [] then
	if endflag then
	  evaluator none board color
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
    if depth = 0 then
      evaluator ocolor board color
    else
      let ms = valid_moves board ocolor in
      if ms = [] then
	if endflag then
	  evaluator none board color
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
		    let try_s = search_enemy nextboard false (-99999) 99999 (depth - 1) in
		    if max_s <= try_s then
		      if try_s < cut then
			search_firstmove board ms try_s (Mv (i, j)) depth
		      else
			(try_s, Mv (i, j))
		    else
		      search_firstmove board ms max_s max_hand depth
  in
  search_firstmove board (valid_moves board color) (-99999) Pass depth

let search_endstage board color refresh cut =
  let vm = valid_moves board color in
  let _ = print_human board color vm in
  print_endline "[END STAGE] Computing all possible moves...";
  let (max_s, max_hand) = search_alphabeta board color (-1) (fun _ -> refresh (); count) cut
  in
  print_string "Expectation: ";
  print_int max_s;
  print_endline "";
  max_hand

let search_theory hist board color =
  let len = String.length hist in
  let rec search theory =
    match theory with
      [] ->
	print_endline "[EARLY STAGE] No theory found";
	None
    | t::ts ->
      if theory_enroute t hist len then
	( print_endline "[EARLY STAGE] I know theory!";
	  Some (theory_decode t.[len]) )
      else search ts
  in search theory_opening

let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)

let get_bit (s, n) (i, j) =
  let n = i + j * 8 in
  let n_bit = Int64.shift_right_logical Int64.min_int n in
  Int64.logand s n_bit <> Int64.zero

let set_bit (s, num) (i, j) =
  let n = i + j * 8 in
  let n_bit = Int64.shift_right_logical Int64.min_int n in
  if Int64.logand s n_bit <> Int64.zero then (s, num)
  else (Int64.logor s n_bit, num + 1)

let count_stable board color =
  let phase1 s =
    let rec along_edge (i, j) (di, dj) s =
      if valid_coordinates (i, j) && get_color board (i, j) = color then
	along_edge (i + di, j + dj) (di, dj) (set_bit s (i, j))
      else
	s
    in
    let s = along_edge (0, 0) (1, 0) s in
    let s = along_edge (0, 0) (0, 1) s in
    let s = along_edge (0, 7) (1, 0) s in
    let s = along_edge (0, 7) (0, -1) s in
    let s = along_edge (7, 0) (-1, 0) s in
    let s = along_edge (7, 0) (0, 1) s in
    let s = along_edge (7, 7) (-1, 0) s in
    let s = along_edge (7, 7) (0, -1) s in
    s
  in
  let phase2 s =
    let rec filled l = match l with [] -> true | car::cdr -> get_color board car <> none && filled cdr in
    let rec allstableandmycolor s l = match l with [] -> true | car::cdr -> get_color board car = color && get_bit s car && allstableandmycolor s cdr in
    let check_row s (r1, r2) = (filled r1 && filled r2) || (allstableandmycolor s r1 && filled r2) || (filled r1 && allstableandmycolor s r2) in
    let make_row (i, j) (di, dj) =
      let rec f (i, j) (di, dj) = if valid_coordinates (i, j) then (i, j) :: f (i + di, j + dj) (di, dj) else [] in
      (f (i - di, j - dj) (-di, -dj), f (i + di, j + dj) (di, dj)) in
    let check s pos = check_row s (make_row pos (1, -1)) && check_row s (make_row pos (1, 0)) && check_row s (make_row pos (1, 1)) && check_row s (make_row pos (0, 1)) in
    let rec phase2' s n =
      if n = 64 then s
      else let pos = (n / 8, n mod 8) in
	   phase2' (if check s pos then (set_bit s pos) else s) (n + 1)
    in phase2' s 0
  in
  let (_, s) = phase2 (phase2 (phase2 (phase1 (0L, 0)))) in s

let evaluator_position board color =
  let scoretable = [-1; -3; -1; 0; 0; -1; -3; -1;
		    4; -1; 2; -1; -1; 2; -1; 4;
		    -11; -16; -1; -3; -3; -1; -16; -11;
		    45; -11; 4; -1; -1; 4; -11; 45] in
  let scoretable = List.rev_append scoretable scoretable in
  let rec iter st n =
    match st with
      [] -> 0
    | s::st ->
      let (i, j) = (n / 8, n mod 8) in
      let c = get_color board (i, j) in
      (if c = color then 1 else if c = none then 0 else -1) * s + iter st (n + 1)
  in
  iter scoretable 0

let evaluator_middlestage refresh next board color =
  refresh ();
  let ocolor = opposite_color color in
  let mycount = count board color in
  let opcount = count board ocolor in
  if next = none then
    if mycount > opcount then
      10000 + mycount - opcount
    else
      if mycount = opcount then
	0
      else
	-10000 + mycount - opcount
  else
    let mypossibility = List.length (valid_moves board color) in
    let oppossibility = List.length (valid_moves board ocolor) in
    let mystable = count_stable board color in
    let opstable = count_stable board ocolor in
    evaluator_position board color
    + (mystable - opstable) * 60
    + (mypossibility - oppossibility) * 70


let search_middlestage phase board color refresh =
  let vm = valid_moves board color in
  let _ = print_human board color vm in
  let depth = phase / 25 + 2 in
  print_string "[MIDDLE STAGE] Trying some of possible future (depth: ";
  print_int depth;
  print_endline ") ...";
  let (max_evaluation, max_hand) = search_alphabeta board color depth (evaluator_middlestage refresh) 99999999
  in
  print_string "Evaluation: ";
  print_int max_evaluation;
  print_endline "";
  max_hand

let play history_code board color refresh =
  let phase = phase board in
  print_string "Phase: ";
  print_int phase;
  print_endline "";
  let time_start = Unix.gettimeofday () in
  let result =
    if phase = 4 then
      Mv (5, 4)
    else if phase > 49 then
      search_endstage board color refresh 33
    else
      match search_theory history_code board color with
	None -> search_middlestage phase board color refresh
      | Some mv -> mv
  in
  Printf.printf "%f seconds.\n" (Unix.gettimeofday () -. time_start);
  result

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

open Array
open Color
open Command
open Theory

type board = int64 * int64 * int * int

let init_board () : board =
  (34628173824L, 68853694464L, 2, 2)

let popcnt (b : int64) =
  let rec popcnt' b cnt =
    if b = 0L then
      cnt
    else
      popcnt' (Int64.shift_right_logical b 1) (if Int64.logand b 1L <> 0L then cnt + 1 else cnt)
  in
  popcnt' b 0

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

let flip (board_b, board_w, count_b, count_w) color n =
  let board_me = if color = black then board_b else board_w in
  let board_op = if color = white then board_b else board_w in
  let n_bit = Int64.shift_right_logical Int64.min_int n in
  let flip' shifter mask =
    let w = if mask then Int64.logand board_op 9114861777597660798L else board_op in
    let t = Int64.logand w (shifter n_bit) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    if Int64.logand board_me (shifter t) <> 0L then t else 0L
  in
  let flipboard = List.fold_left2 (fun b s m -> Int64.logor b (flip' s m)) 0L
    [(fun n -> Int64.shift_left n 9); (fun n -> Int64.shift_left n 8); (fun n -> Int64.shift_left n 7); (fun n -> Int64.shift_left n 1);
     (fun n -> Int64.shift_right_logical n 1); (fun n -> Int64.shift_right_logical n 7); (fun n -> Int64.shift_right_logical n 8); (fun n -> Int64.shift_right_logical n 9)] [true;false;true;true;true;true;false;true] in
  (Int64.logor n_bit flipboard, popcnt flipboard)

let doMove (board_b, board_w, count_b, count_w) com color =
  match com with
    GiveUp  -> (board_b, board_w, count_b, count_w)
  | Pass    -> (board_b, board_w, count_b, count_w)
  | Mv (i, j) ->
    let (f, n) = flip (board_b, board_w, count_b, count_w) color (i + j * 8) in
    if color = black then
      (Int64.logor board_b f, Int64.logand board_w (Int64.lognot f), count_b + 1 + n, count_w - n)
    else
      (Int64.logand board_b (Int64.lognot f), Int64.logor board_w f, count_b - n, count_w + 1 + n)

let valid_moves (board_b, board_w, count_b, count_w) color =
  let board_me = if color = black then board_b else board_w in
  let board_op = if color = white then board_b else board_w in
  let blank = Int64.lognot (Int64.logor board_b board_w) in
  let valid_moves' shifter mask =
    let w = if mask then Int64.logand board_op 9114861777597660798L else board_op in
    let t = Int64.logand w (shifter board_me) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    let t = Int64.logor t (Int64.logand w (shifter t)) in
    shifter t
  in
  let rec board2list b n l =
    if n < 0 then l
    else board2list (Int64.shift_right_logical b 1) (n - 1) (if Int64.logand b 1L <> 0L then n :: l else l)
  in
  board2list (Int64.logand blank (List.fold_left2 (fun b s m -> Int64.logor b (valid_moves' s m)) 0L
  				    [(fun n -> Int64.shift_left n 9); (fun n -> Int64.shift_left n 8); (fun n -> Int64.shift_left n 7); (fun n -> Int64.shift_left n 1);
  				     (fun n -> Int64.shift_right_logical n 1); (fun n -> Int64.shift_right_logical n 7); (fun n -> Int64.shift_right_logical n 8); (fun n -> Int64.shift_right_logical n 9)] [true;false;true;true;true;true;false;true])) 63 []


let print_human board color valid_moves =
  let counter = ref 1 in
  (* print_string "\x1b[2J"; *)
  for j = 0 to 7 do
    for i = 0 to 7 do
      if List.mem (i + j * 8) valid_moves then
	( if color = white then Printf.printf "\x1b[37;46m%2x\x1b[m" !counter
	  else                  Printf.printf "\x1b[30;46m%2x\x1b[m" !counter ;
	  counter := !counter + 1 )
      else
	let c = get_color board (i, j) in
	if c = white then print_string "\x1b[01;37;42m● \x1b[m"
	else if c = black then print_string "\x1b[01;30;42m● \x1b[m"
	else print_string "\x1b[01;33;42m+ \x1b[m"
    done;
    print_endline ""
  done


let count (board_b, board_w, count_b, count_w) color =
  if color = black then count_b else count_w

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
	  | n::ms -> let nextboard = doMove board (Mv (n mod 8, n / 8)) color in
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
	  | n::ms -> let nextboard = doMove board (Mv (n mod 8, n / 8)) ocolor in
		     let beta = min beta (search_me nextboard false alpha beta (depth - 1)) in
		     if alpha >= beta then alpha else search ms beta
	in
	search ms beta
  in
  let rec search_firstmove board ms max_s max_hand depth =
    match ms with
      [] -> (max_s, max_hand)
    | n::ms -> let nextboard = doMove board (Mv (n mod 8, n / 8)) color in
	       let try_s = search_enemy nextboard false (-99999) 99999 (depth - 1) in
	       if max_s <= try_s then
		 if try_s < cut then
		   search_firstmove board ms try_s (Mv (n mod 8, n / 8)) depth
		 else
		   (try_s, Mv (n mod 8, n / 8))
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

let stable (board_b, board_w, count_b, count_w) color =
  let board = Int64.logor board_b board_w in
  let myboard = if color = black then board_b else board_w in
  let stable' stable_board =
    let unflippable n upper lower =
      let f t = Int64.logand board (Int64.logor t (Int64.shift_right_logical t n)) in
      let g t = Int64.logand board (Int64.logor t (Int64.shift_left t n)) in
      let upper_earthed = f (f (f (f (f (f (f (Int64.logand board upper))))))) in
      let lower_earthed = g (g (g (g (g (g (g (Int64.logand board lower))))))) in
      let f t = Int64.logand myboard (Int64.logor t (Int64.shift_right_logical t n)) in
      let g t = Int64.logand myboard (Int64.logor t (Int64.shift_left t n)) in
      let upper_earthed2 = f (f (f (f (f (f (f (Int64.logand myboard (Int64.logor upper stable_board)))))))) in
      let lower_earthed2 = g (g (g (g (g (g (g (Int64.logand myboard (Int64.logor lower stable_board)))))))) in
      Int64.logor (Int64.logor (Int64.logand upper_earthed lower_earthed) upper_earthed2) lower_earthed2
    in
    let unflippable_NW_SE =
      let f t = Int64.logand board (Int64.logor t (Int64.shift_right_logical (Int64.logand t (-72340172838076928L)) 9)) in
      let g t = Int64.logand board (Int64.logor t (Int64.shift_left (Int64.logand t 35887507618889599L) 9)) in
      let upper_earthed = f (f (f (f (f (f (f (Int64.logand board (-35887507618889600L)))))))) in
      let lower_earthed = g (g (g (g (g (g (g (Int64.logand board 72340172838076927L))))))) in
      let f t = Int64.logand myboard (Int64.logor t (Int64.shift_right_logical (Int64.logand t (-72340172838076928L)) 9)) in
      let g t = Int64.logand myboard (Int64.logor t (Int64.shift_left (Int64.logand t 35887507618889599L) 9)) in
      let upper_earthed2 = f (f (f (f (f (f (f (Int64.logand myboard (Int64.logor (-35887507618889600L) stable_board)))))))) in
      let lower_earthed2 = g (g (g (g (g (g (g (Int64.logand myboard (Int64.logor 72340172838076927L stable_board)))))))) in
      Int64.logor (Int64.logor (Int64.logand upper_earthed lower_earthed) upper_earthed2) lower_earthed2
    in
    let unflippable_NE_SW =
      let f t = Int64.logand board (Int64.logor t (Int64.shift_right_logical (Int64.logand t 9187201950435737344L) 7)) in
      let g t = Int64.logand board (Int64.logor t (Int64.shift_left (Int64.logand t 71775015237779198L) 7)) in
      let upper_earthed = f (f (f (f (f (f (f (Int64.logand board (-71775015237779199L)))))))) in
      let lower_earthed = g (g (g (g (g (g (g (Int64.logand board (-9187201950435737345L)))))))) in
      let f t = Int64.logand myboard (Int64.logor t (Int64.shift_right_logical (Int64.logand t 9187201950435737344L) 7)) in
      let g t = Int64.logand myboard (Int64.logor t (Int64.shift_left (Int64.logand t 71775015237779198L) 7)) in
      let upper_earthed2 = f (f (f (f (f (f (f (Int64.logand myboard (Int64.logor (-71775015237779199L) stable_board)))))))) in
      let lower_earthed2 = g (g (g (g (g (g (g (Int64.logand myboard (Int64.logor (-9187201950435737345L) stable_board)))))))) in
      Int64.logor (Int64.logor (Int64.logand upper_earthed lower_earthed) upper_earthed2) lower_earthed2
    in
    Int64.logor stable_board (Int64.logand (Int64.logand (unflippable 8 (-72057594037927936L) 255L) (unflippable 1 (-9187201950435737472L) 72340172838076673L)) (Int64.logand unflippable_NW_SE unflippable_NE_SW))
  in
  let rec infinite s =
    let s' = stable' s in
    if s <> s' then infinite s' else s
  in
  infinite 0L


let evaluator_position board color =
  let scoretable = [-1; -3; -1; 0; 0; -1; -3; -1;
		    4; -1; 2; -1; -1; 2; -1; 4;
		    -11; -16; -1; -3; -3; -1; -16; -11;
		    45; -11; 4; -1; -1; 4; -11; 45] in
  let scoretable = List.rev_append scoretable scoretable in
  let rec iter st n m =
    match st with
      [] -> m
    | s::st ->
      let (i, j) = (n mod 8, n / 8) in
      let c = get_color board (i, j) in
      iter st (n + 1) (if c = color then m + s else if c = none then m else m - s)
  in
  iter scoretable 0 0

let evaluator_middlestage refresh next (board_b, board_w, count_b, count_w) color =
  refresh ();
  let ocolor = opposite_color color in
  let mycount = count (board_b, board_w, count_b, count_w) color in
  let opcount = count (board_b, board_w, count_b, count_w) ocolor in
  if next = none then
    if mycount > opcount then
      10000 + mycount - opcount
    else
      if mycount = opcount then
	0
      else
	-10000 + mycount - opcount
  else
    let mystable = popcnt (stable (board_b, board_w, count_b, count_w) color) in
    let opstable = popcnt (stable (board_b, board_w, count_b, count_w) ocolor) in
    evaluator_position (board_b, board_w, count_b, count_w) color
    + (mystable - opstable) * 400


let search_middlestage phase board color refresh depth =
  let vm = valid_moves board color in
  let _ = print_human board color vm in
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
    else if phase > 48 then
      search_endstage board color refresh 33
    else
      match search_theory history_code board color with
	None -> search_middlestage phase board color refresh 5
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


(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

let rec contains l e = match l with
	| h :: t -> if h = e then true else contains t e
	| _ -> false

(* Decently object-oriented model of the problem *)
type grid_cell_state = Open | Blocked | OutOfBounds

type direction = Left | Right | Up | Down

type guard = {
  mutable xpos : int;
  mutable ypos : int;
  mutable facing : direction
}

let turn_guard g = match g.facing with
	| Left -> g.facing <- Up
	| Right -> g.facing <- Down
	| Up -> g.facing <- Right
	| Down -> g.facing <- Left

let next_guard_pos g = match g.facing with
	| Left -> (g.xpos - 1, g.ypos)
	| Right -> (g.xpos + 1, g.ypos)
	| Up -> (g.xpos, g.ypos - 1)
	| Down -> (g.xpos, g.ypos + 1)

let move_guard g pos = g.xpos <- (fst pos) ; g.ypos <- (snd pos)


(* grid will be a 2D Array of chars *)
let get_grid_cell_state g pos = let ymax = Array.length g in
	let xmax = Array.length (g.(0)) in
	if (fst pos) < 0 || (fst pos) >= xmax || (snd pos) < 0 || (snd pos) >= ymax
	then OutOfBounds
	else if g.(snd pos).(fst pos) = '.' then Open else Blocked

let map_guard = { xpos = 0 ; ypos = 0; facing = Up }

let map_builder c guard x y = if (c='^')
	then (guard.xpos <- x ; guard.ypos <- y ; '.')
	else c

let map_grid = Array.init_matrix (String.length (List.hd file_lines)) (List.length file_lines)
	(fun x y -> map_builder ((List.nth file_lines x).[y]) map_guard y x)

let orig_guard_pos = (map_guard.xpos, map_guard.ypos)
let guard_visited = ref []


(* Keep stepping the guard until they leave the map *)
let () = while (get_grid_cell_state map_grid (map_guard.xpos, map_guard.ypos)) <> OutOfBounds do
	(* Add this position to the list of positions the guard has visited *)
	if not (contains !guard_visited (map_guard.xpos, map_guard.ypos)) 
	then guard_visited := (map_guard.xpos, map_guard.ypos) :: !guard_visited
	else () ;
	let next_pos = next_guard_pos map_guard in
	match get_grid_cell_state map_grid next_pos with
		| Blocked -> turn_guard map_guard
		| _ -> move_guard map_guard next_pos
done

let () = Printf.printf "Part 1: %d\n" (List.length !guard_visited)


(* Part 2: Just brute force adding an extra obstacle to each cell that isn't the original guard position 
   and isn't already an obstacle. The guard is stuck in a loop if they ever reach the same location facing 
   the same direction twice. If we wanted to be really lazy we could pigeonhole principle and say a loop 
   must be happening if the guard hasn't gone out of bounds after 4 * length * width steps *)


let guard_stuck grid guard = 
	let visited = ref [] in
	let stuck = ref false in
	while (not !stuck) && ((get_grid_cell_state grid (guard.xpos, guard.ypos)) <> OutOfBounds) do
		(* Track position and facing this time *)
		if not (contains !visited (guard.xpos, guard.ypos, guard.facing))
		then visited := (guard.xpos, guard.ypos, guard.facing) :: !visited
		else stuck := true ;
		let next_pos = next_guard_pos guard in
		match get_grid_cell_state grid next_pos with
			| Blocked -> turn_guard guard
			| _ -> move_guard guard next_pos
	done ;
	!stuck

let stuck_count = ref 0

let () = for x = 0 to (Array.length map_grid.(0)) - 1 do
	for y = 0 to (Array.length map_grid) - 1 do
		(* Reset guard position and facing *)
		move_guard map_guard orig_guard_pos ;
		map_guard.facing <- Up ;
		(* Put an obstacle at the current position, if it isn't already one or isn't where the guard starts *)
		if (not ((x,y) = orig_guard_pos)) && (not ((get_grid_cell_state map_grid (x,y)) = Blocked)) 
		then (
			Array.set map_grid.(y) x '#' ;
			if guard_stuck map_grid map_guard then stuck_count := !stuck_count + 1 else () ;
			Array.set map_grid.(y) x '.'
		)
		else ()
	done
done

let () = Printf.printf "Part 2: %d\n" !stuck_count

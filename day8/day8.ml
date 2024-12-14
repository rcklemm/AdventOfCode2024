(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

let max_row = List.length file_lines
let max_col = String.length (List.hd file_lines)

(* want to construct a map of letter -> list of locations *)
let append_to_map map c x y = if List.mem_assoc c map
	then ((List.assoc c map) := (x, y) :: !(List.assoc c map) ; map)
	else (c, (ref [(x, y)])) :: map

let letter_loc_map_mut = ref [];;

List.iteri (fun y row -> 
	(List.iteri (fun x c -> if c = '.' 
				then () 
				else (letter_loc_map_mut := append_to_map !letter_loc_map_mut c x y)
		) row
	)
) (List.map (fun s -> s |> String.to_seq |> List.of_seq) file_lines)

(* de-mutabalize now that we are done parsing the input *)
let letter_loc_map = List.map (fun x -> ((fst x), !(snd x))) !letter_loc_map_mut

(* find every antinode within the bounds of the map *)
let gen_antinode node1 node2 = 
	let slope = ((fst node2) - (fst node1), (snd node2) - (snd node1)) in
	((fst node2) + (fst slope), (snd node2) + (snd slope))

let rec gen_list elem l = match l with
	| h :: t -> (gen_antinode elem h) :: (gen_antinode h elem) :: (gen_list elem t)
	| _ -> []

let rec gen_all_pairs l = match l with
	| h :: t -> (gen_list h t) @ (gen_all_pairs t)
	| _ -> []

let antinode_valid xmax ymax node = let x = (fst node) in
	let y = (snd node) in
	(x >= 0) && (x < xmax) && (y >= 0) && (y < ymax)

let all_antinodes = List.fold_left ( @ ) [] (List.map (fun x -> gen_all_pairs (snd x)) letter_loc_map)

let valid_antinodes = List.filter (antinode_valid max_row max_col) (List.sort_uniq compare all_antinodes)

let () = Printf.printf "Part 1: %d\n" (List.length valid_antinodes)

(* part 2: antinodes happen in a line instead of one spot *)

let rec gen_antinode2 node1 node2 xmax ymax = if antinode_valid xmax ymax node2
	then let node3 = gen_antinode node1 node2 in
		node3 :: (gen_antinode2 node2 node3 xmax ymax)
	else []

let rec gen_list2 elem l = match l with
	| h :: t -> (gen_antinode2 elem h max_row max_col) @ (gen_antinode2 h elem max_row max_col) @ (gen_list2 elem t)
	| _ -> []

let rec gen_all_pairs2 l = match l with
	| h :: t -> [h] @ (gen_list2 h t) @ (gen_all_pairs2 t)
	| _ -> []

let all_antinodes2 = List.fold_left ( @ ) [] (List.map (fun x -> gen_all_pairs2 (snd x)) letter_loc_map)
let valid_antinodes2 = List.filter (antinode_valid max_row max_col) (List.sort_uniq compare all_antinodes2)

let () = Printf.printf "Part 2: %d\n" (List.length valid_antinodes2)

 

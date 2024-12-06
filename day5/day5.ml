(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

(* Rules have a '|' but no ',' and updates have a ',' but no '|' *)
let rule_lines = List.filter (fun s -> String.contains s '|') file_lines
let update_lines = List.filter (fun s -> String.contains s ',') file_lines

let rule_str_to_pair s = let split = (String.split_on_char '|' s) in
	(int_of_string (List.hd split), int_of_string (List.hd (List.tl split)))

(* Keep the list sorted by the 'after' field for now *)
let rules = List.sort 
	(fun x y -> if (snd x) = (snd y) then (fst x) - (fst y) else (snd x) - (snd y)) 
	(List.map rule_str_to_pair rule_lines)

let rec build_rules_map r lasta cur_set = match r with
	| h :: t -> let a = (snd h) in if a = lasta
		then build_rules_map t a ((fst h) :: cur_set)
		else (lasta, cur_set) :: build_rules_map t a [(fst h)]
	| _ -> [(lasta, cur_set)]

let rules_map = build_rules_map rules (snd (List.hd rules)) []

let rec contains l e = match l with
	| h :: t -> if h = e then true else contains t e
	| _ -> false

let updates = List.map (List.map int_of_string) (List.map (String.split_on_char ',') update_lines)

let rec update_valid u invalids = match u with
	| h :: t -> if contains invalids h then false 
		else (match List.assoc_opt h rules_map with
			| Some l -> update_valid t (l @ invalids)
			| None -> update_valid t invalids)
	| _ -> true

let get_middle l = List.nth l (List.length l / 2)

let valid_updates = List.filter (fun u -> update_valid u []) updates

let sum_valid_middles = List.fold_left ( + ) 0 (List.map get_middle valid_updates)
let () = Printf.printf "Part 1: %d\n" sum_valid_middles


(* Part 2: need to correctly order the updates now. Ideally we can parse the rules in some way that forms
   a total ordering on the set of page numbers. Then we can just make a custom comparator and sort the 
   update list into the correct order.

   The transitive closure of the (before, after) rules pairs should be the ordering that we want. Giving up
   on strictly functional programming and just implementing Floyd-Warshal imperatively
*)

(* All the page numbers are 2 digits, so a 100x100 array is enough. *)
let rules_matrix = Array.make_matrix 100 100 0

(* Iterate over rules and initialize the matrix *)
let rec init_matrix r m = match r with
	| h :: t -> Array.set m.(fst h) (snd h) 1 ; init_matrix t m
	| _ -> ()

(* Floyd-Warshal algorithm to compute transitive closure *)
let () = for k = 1 to 99 do 
	for i = 1 to 99 do
		for j = 1 to 99 do
			let s = (rules_matrix.(i).(k) + rules_matrix.(k).(j)) in
			if rules_matrix.(i).(j) > s then
				Array.set rules_matrix.(i) j s
			else ()
		done
	done
done

(* Custom sorting comparator based on the transitive closure *)
let matrix_cmp m r x y = if contains r (x,y) then -1
	else if contains r (y,x) then 1
	else 0

let invalid_updates = List.filter (fun u -> not (update_valid u [])) updates

let fixed_updates = List.map (List.sort (matrix_cmp rules_matrix rules)) invalid_updates

let sum_fixed_middles = List.fold_left ( + ) 0 (List.map get_middle fixed_updates)

let () = Printf.printf "Part 2: %d\n" sum_fixed_middles


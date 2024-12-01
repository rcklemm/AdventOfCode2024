(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

(* Lines are number, 3 spaces, number *)
let line_first_int line = int_of_string (match (Str.split (Str.regexp "   ") line) with
						| [f; s] -> f
						| _ -> "")

let line_second_int line = int_of_string (match (Str.split (Str.regexp "   ") line) with
						| [f; s] -> s
						| _ -> "")

let listA = List.sort compare (List.map line_first_int file_lines)
let listB = List.sort compare (List.map line_second_int file_lines)

(* Compute the part 1 operation, L1 norm of listA - listB *)
let rec sum_diffs a b = 
	match a, b with
	 | ha :: ta, hb :: tb -> abs(ha - hb) + sum_diffs ta tb
  	 | _ -> 0

(* Output the part 1 result. Need to assign to void to get the compiler to parse it right *)
let () = Printf.printf "Part 1: %d\n" (sum_diffs listA listB)

(* Compute the part 2 operation, number of times each left element appears in right *)
(* Technically since the lists are already sorted this could be made way faster by
   managing two iterators:
     - 1. Set count = 0
     - 2. increment count if *iterA == *iterB, step iterB
     - 3. if *iterB > *iterA, process count, step iterA, goto 1. Else, goto 2.
   Not sure how I would turn this from imperative to functional programming style *)
let rec count_occur l e =
	match l with
	 | [] -> 0
	 | h :: t -> (if h = e then 1 else 0) + (count_occur t e)

let listCounts = List.map (count_occur listB) listA

(* Output the part 2 result *)
let () = Printf.printf "Part 2: %i\n" (List.fold_left ( + ) 0 (List.map2 ( * ) listA listCounts))


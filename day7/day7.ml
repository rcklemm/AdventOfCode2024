(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

(* Return pair of (target, numlist) from the line *)
let parse_line line = let col_split = (String.split_on_char ':' line) in
	(int_of_string (List.hd col_split), List.map (int_of_string)
		(List.tl (String.split_on_char ' ' (List.hd (List.tl col_split)))))

let calibrations = List.map parse_line file_lines

(* 2^(n-1) options for operations, which can be packed into an integer *)
let rec int_to_ops i bits = if bits = 0 then [] else
	match Int.logand i 1 with
	 | 0 -> ( + ) :: int_to_ops (Int.shift_right_logical i 1) (bits - 1)
	 | _ -> ( * ) :: int_to_ops (Int.shift_right_logical i 1) (bits - 1)

let rec apply_ops nums ops = match nums with
	| a :: b :: t -> apply_ops (((List.hd ops) a b) :: t) (List.tl ops)
	| a :: [] -> a
	| _ -> 0

let rec try_all_ops_aux target nums bits i =
	let this_val = apply_ops nums (int_to_ops i bits) in
	if this_val = target then true
	else if i = 0 then false
	else try_all_ops_aux target nums bits (i - 1)


let try_all_ops cal = let target = (fst cal) in
	let nums = (snd cal) in
	let bits = (List.length nums) - 1 in
	try_all_ops_aux target nums bits ((Int.shift_left 1 bits) - 1)

let true_cals = List.filter try_all_ops calibrations
let sum_true = List.fold_left ( + ) 0 (List.map fst true_cals)

let () = Printf.printf "Part 1: %d\n" sum_true


(* Part 2: Add another operation, rework operator generation *)
let concat a b = int_of_string ( (string_of_int a ) ^ (string_of_int b) )

(* Ocaml doesnt do integer exponentiation, so implement a (slow) version *)
let rec pow_int a b = if b = 0 then 1 else a * (pow_int a (b - 1))

(* Now we just pack everything as a base-3 integer *)
let rec int_to_ops2 i trits = if trits = 0 then [] else
	match (i mod 3) with
	 | 0 -> ( + ) :: int_to_ops2 (i / 3) (trits - 1)
	 | 1 -> ( * ) :: int_to_ops2 (i / 3) (trits - 1)
	 | _ -> ( concat ) :: int_to_ops2 (i / 3) (trits - 1)

let rec try_all_ops_aux2 target nums trits i = 
	let this_val = apply_ops nums (int_to_ops2 i trits) in
	if this_val = target then true
	else if i = 0 then false
	else try_all_ops_aux2 target nums trits (i - 1)

let try_all_ops2 cal = let target = (fst cal) in
	let nums = (snd cal) in
	let trits = (List.length nums) - 1 in
	try_all_ops_aux2 target nums trits ((pow_int 3 trits) - 1)

let true_cals2 = List.filter try_all_ops2 calibrations
let sum_true2 = List.fold_left ( + ) 0 (List.map fst true_cals2)

let () = Printf.printf "Part 2: %d\n" sum_true2


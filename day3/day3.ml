(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

(* Could definitely do regex but lets try to write a simple scanner *)
type scanner_states =
	| NEXT_MUL
	| NEXT_OPEN_PAREN
	| NEXT_INT1
	| NEXT_COMMA
	| NEXT_INT2
	| NEXT_CLOSE_PAREN

let string_to_char_list s = s |> String.to_seq |> List.of_seq

let program = List.concat (List.map string_to_char_list file_lines)

let rec pop_n l n = if (n > 0) then 
	match l with
	 | h :: t -> pop_n t (n - 1)
	 | _ -> []
	else l

let token_mul l = match l with
	| c1 :: c2 :: c3 :: t -> (c1='m') && (c2='u') && (c3='l')
	| _ -> false

let token_open_paren l = match l with
	| c :: t -> c = '('
	| _ -> false

let token_comma l = match l with
	| c :: t -> c = ','
	| _ -> false

let token_close_paren l = match l with
	| c :: t -> c = ')'
	| _ -> false

let is_digit c = ((Char.code c) >= (Char.code '0')) && ((Char.code c) <= (Char.code '9'))

let rec token_num l s_num = match l with
	| c :: t -> if is_digit c then s_num ^ (token_num t (String.make 1 c)) else s_num
	| _ -> s_num

let valid_num s_num = ((String.length s_num) > 0) && ((String.length s_num) < 4)

let rec scan_line l state i1 i2 = if (List.compare_length_with l 0) = 0 then [] else match state with
	| NEXT_MUL -> if token_mul l 
			then scan_line (pop_n l 3) NEXT_OPEN_PAREN i1 i2 
			else scan_line (List.tl l) NEXT_MUL 0 0
	| NEXT_OPEN_PAREN -> if token_open_paren l
			then scan_line (List.tl l) NEXT_INT1 i1 i2
			else scan_line l NEXT_MUL 0 0
	| NEXT_INT1 -> let s = token_num l "" in (if valid_num s
			then scan_line (pop_n l (String.length s)) NEXT_COMMA (int_of_string s) i2
			else scan_line l NEXT_MUL 0 0)
	| NEXT_COMMA -> if token_comma l
			then scan_line (List.tl l) NEXT_INT2 i1 i2
			else scan_line l NEXT_MUL 0 0
	| NEXT_INT2 -> let s = token_num l "" in (if valid_num s
			then scan_line (pop_n l (String.length s)) NEXT_CLOSE_PAREN i1 (int_of_string s)
			else scan_line l NEXT_MUL 0 0)
	| NEXT_CLOSE_PAREN -> if token_close_paren l
			then (i1,i2) :: scan_line (List.tl l) NEXT_MUL 0 0
			else scan_line l NEXT_MUL 0 0

(* Collect all the pairs we pulled from the multiplication *)
let mul_pairs = scan_line program NEXT_MUL 0 0

(* Compute part 1 solution *)
let () = Printf.printf "Part 1: %d\n" (List.fold_left ( + ) 0 (List.map (fun x -> (fst x) * (snd x)) mul_pairs))

(* Part 2: Need to handle do() and don't() instructions throughout the input *)
(* Modify our scanning algorithm: same loop as before, but if we are at a don't() at the top of the function,
   immediately seek to the end of the next do() *)
let token_do l = match l with
	| c1 :: c2 :: c3 :: c4 :: t -> (c1='d') && (c2='o') && (c3='(') && (c4=')')
	| _ -> false

let token_dont l = match l with
	| c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: t ->
		(c1='d') && (c2='o') && (c3='n') && (c4='\'') && (c5='t') && (c6='(') && (c7=')')
	| _ -> false

let rec find_next_do l = 
	if token_do l then pop_n l 4 
	else if (List.compare_length_with l 0) = 0 then []
	else find_next_do (List.tl l)


let rec scan_line_2 l state i1 i2 = if token_dont l then scan_line_2 (find_next_do l) NEXT_MUL 0 0 else
	if (List.compare_length_with l 0) = 0 then [] else match state with
	| NEXT_MUL -> if token_mul l 
			then scan_line_2 (pop_n l 3) NEXT_OPEN_PAREN i1 i2 
			else scan_line_2 (List.tl l) NEXT_MUL 0 0
	| NEXT_OPEN_PAREN -> if token_open_paren l
			then scan_line_2 (List.tl l) NEXT_INT1 i1 i2
			else scan_line_2 l NEXT_MUL 0 0
	| NEXT_INT1 -> let s = token_num l "" in (if valid_num s
			then scan_line_2 (pop_n l (String.length s)) NEXT_COMMA (int_of_string s) i2
			else scan_line_2 l NEXT_MUL 0 0)
	| NEXT_COMMA -> if token_comma l
			then scan_line_2 (List.tl l) NEXT_INT2 i1 i2
			else scan_line_2 l NEXT_MUL 0 0
	| NEXT_INT2 -> let s = token_num l "" in (if valid_num s
			then scan_line_2 (pop_n l (String.length s)) NEXT_CLOSE_PAREN i1 (int_of_string s)
			else scan_line_2 l NEXT_MUL 0 0)
	| NEXT_CLOSE_PAREN -> if token_close_paren l
			then (i1,i2) :: scan_line_2 (List.tl l) NEXT_MUL 0 0
			else scan_line_2 l NEXT_MUL 0 0

(* Collect all the pairs we pulled from the multiplication *)
let mul_pairs2 = scan_line_2 program NEXT_MUL 0 0 

(* Compute part 2 solution *)
let () = Printf.printf "Part 2: %d\n" (List.fold_left ( + ) 0 (List.map (fun x -> (fst x) * (snd x)) mul_pairs2))


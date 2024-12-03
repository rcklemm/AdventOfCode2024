(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

(* Each 'level' in a 'report' is an int, levels are separated by spaces within the line holding the report *)
let report_list = List.map (List.map int_of_string) (List.map (Str.split (Str.regexp " ")) file_lines)

let inc_check a b = (a < b) && ((b - a) <= 3)
let dec_check a b = (a > b) && ((a - b) <= 3)


let rec inc_check_list l = 
	match l with 
	 | a :: b :: t -> if inc_check a b then inc_check_list (b :: t) else false
	 | _ -> true

let rec dec_check_list l = 
	match l with
	 | a :: b :: t -> if dec_check a b then dec_check_list (b :: t) else false
	 | _ -> true

(* determine if it is increasing/decreasing by the first pair, then send to inc/dec_check *)
let safety_check l = 
	match l with
	 | a :: b :: t -> if (a > b) then dec_check_list l else inc_check_list l
	 | _ -> true

(* Count the number of safe reports *)
let num_safe = List.length (List.filter safety_check report_list)
let () = Printf.printf "Part 1: %d\n" num_safe



(* 
   Part 2: The basic idea behind optimizing this pass is this: in part 1 we do a sequence
   of checks check(a_1, a_2) && check(a_2, a_3) && ... && check(a_{n-1}, a_n),
   all of which must pass for the report to be safe. With the part 2 modification, we can
   allow for one check(a_i, a_{i+1}) to fail, and see if we can recover by:
	1. Special case if i = 0, we don't know whether check is an inc_check or dec_check.
	   So, run safety_check on [a_2, ..., a_n] and [a_1, a_3, ..., a_n]
		1a. There's an extra caveat here that I didn't think of initially. In the case that
		    we remove the second element, we might want to change our minds about whether
		    we use inc_check or dec_check. Since this can only happen at the start, our
		    special case is actually triggered if check(a_1, a_2) or check(a_2, a_3) fails 
	2. Special case if i+1 = n, if this is our only failure, then trivially removing a_n works.
	3. Try removing a_i: continue running the safety check on [a_{i-1}, a_{i+1}, ..., a_n]
	4. Try removing a_{i+1}: continue running the safety check on [a_i, a_{i+2}, ..., a_n]
   Note that once a removal happens, we stop if we see another failure, since we can only tolerate one.
*)


let rec inc_check_list_ext l = 
	(* Need to maintain 3 elements of context, can start with a_2,a_3, since the
	   caller has verified a_1,a_2 is valid *)
	match l with
	 | a :: b :: c :: t -> if inc_check b c then inc_check_list_ext (b :: c :: t)
		else if inc_check_list (a :: c :: t) then true
		else inc_check_list (a :: b :: t)
	 (* If there has been no failure by this point, we are good, worst case we
	    needed to removed a_n *)
	 | a :: b :: [] -> true
	 (* Should never happen but makes the compiler happy *)
	 | _ -> true

let rec dec_check_list_ext l = 
	(* Need to maintain 3 elements of context, can start with a_2,a_3, since the
	   caller has verified a_1,a_2 is valid *)
	match l with
	 | a :: b :: c :: t -> if dec_check b c then dec_check_list_ext (b :: c :: t)
		else if dec_check_list (a :: c :: t) then true
		else dec_check_list (a :: b :: t)
	 (* If there has been no failure by this point, we are good, worst case we
	    needed to removed a_n *)
	 | a :: b :: [] -> true
	 (* Should never happen but makes the compiler happy *)
	 | _ -> true

let safety_check_ext l =
	(* Handle the special case where check(a_1, a_2) fails, then
	   basically use the same strategy as part 1 *)
	match l with
	 | a :: b :: c :: t -> if not ( ( (inc_check a b) && (inc_check b c) ) || ( (dec_check a b) && (dec_check b c) ) )
		then (if safety_check (a :: c :: t) then true else safety_check (b :: c :: t))
		else (if (a > b) then dec_check_list_ext l else inc_check_list_ext l)
	 | _ -> true


(* Count the number of safe reports, under the new definition of safe *)
let num_safe2 = List.length (List.filter safety_check_ext report_list)
let () = Printf.printf "Part 2: %d\n" num_safe2

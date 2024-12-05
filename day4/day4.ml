(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

let string_to_char_list s = s |> String.to_seq |> List.of_seq

let rec count_xmas l = match l with
	| c1 :: c2 :: c3 :: c4 :: t -> if (c1='X') && (c2='M') && (c3='A') && (c4='S') 
		then 1 + count_xmas (c2 :: c3 :: c4 :: t)
		else count_xmas (c2 :: c3 :: c4 :: t)
	| _ -> 0

let rec pop_n n l = if (n > 0) then 
	match l with
	 | h :: t -> pop_n (n - 1) t
	 | _ -> []
	else l

(* 0-indexed, custom for our lists of chars, return a '.' if we are ever out-of-bounds *)
let rec clist_nth n l = match l with
	| h :: t -> if (n > 0) then clist_nth (n - 1) t else h
	| _ -> '.'

let word_search_grid = List.map string_to_char_list file_lines

(* horizontal counts are easy in this format *)
let horiz_count = (List.fold_left ( + ) 0 (List.map count_xmas word_search_grid))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.map List.rev word_search_grid)))

(* to get the vertical counts, need a way to extract columns from the grid *)
let rec transpose_aux i n grid = if (i < n) then
	(List.map (clist_nth i) grid) :: transpose_aux (i+1) n grid
	else []

let transpose grid = transpose_aux 0 (List.length grid) grid

let transpose_word_search = transpose word_search_grid

let vert_count = (List.fold_left ( + ) 0 (List.map count_xmas transpose_word_search))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.map List.rev transpose_word_search)))

(* diagonals are more complicated. There is a decent way to express the upper triangular diagonals of a grid.
   then the upper triangulars of the transpose will be the lower triangulars of the original. Just need to not
   double count the main diagonal. Then if we reverse the grid, we get the diagonals going the other direction,
   i.e. right to left instead of left to right
*)

let rec upper_triangle_aux i n grid = if (i < n) then
	(List.mapi clist_nth (List.map (pop_n i) grid)) :: upper_triangle_aux (i + 1) n grid
	else []

let upper_triangle grid = upper_triangle_aux 0 (List.length grid) grid

let rev_word_search = List.map List.rev word_search_grid
let trev_word_search = transpose rev_word_search

let diag_count = (List.fold_left ( + ) 0 (List.map count_xmas (upper_triangle word_search_grid)))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.map List.rev (upper_triangle word_search_grid))))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.tl (upper_triangle transpose_word_search))))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.tl (List.map List.rev (upper_triangle transpose_word_search)))))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (upper_triangle rev_word_search)))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.map List.rev (upper_triangle rev_word_search))))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.tl (upper_triangle trev_word_search))))
		+ (List.fold_left ( + ) 0 (List.map count_xmas (List.tl (List.map List.rev (upper_triangle trev_word_search)))))

let () = Printf.printf "Part 1: %d\n" (horiz_count + vert_count + diag_count)

(* Part 2: yikes. The obvious imperative solution is to look at each 3x3 sliding window on the grid and see
   if its center is the 'A' in an X-MAS. So, we can try to map the grid to a list of 3x3 subgrids, then
   run an is_xmas checker over those subgrids. Can't think of a good recursive way to do the 3x3 sliding window,
   so let's just do gross indexing math on strings *)

let num_cols = List.length (List.hd word_search_grid)
let num_rows = List.length word_search_grid

let flat_grid_str = (List.fold_left ( ^ ) "" file_lines)

(* it will be nice for us later if this returns strings and not chars *)
let str_safe_index s i = if (i >= 0) && (i < (String.length s)) then (String.make 1 s.[i]) else ".";;

(* We are dealing with a row-major flattened string, so we just need to know the number of columns and a current
   index to find the diagonals touching this entry *)
let get_cross_indices i c = if (i mod c <> 0) && (i mod c <> c - 1) then [i - c - 1; i - c + 1; i; i + c - 1; i + c + 1] else []

let get_cross_at i c s = List.fold_left ( ^ ) "" (List.map (str_safe_index s) (get_cross_indices i c))

let valid_cross s = (s="MSAMS") || (s="MMASS") || (s="SMASM") || (s="SSAMM")

let all_crosses = List.init (num_cols * num_rows) (fun i -> get_cross_at i num_cols flat_grid_str)

let cross_count = List.length (List.filter valid_cross all_crosses)

let () = Printf.printf "Part 2: %d\n" cross_count

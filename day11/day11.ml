(* Use Jane Street core library to get read_lines functionality *)
open Stdio.In_channel

let file_lines = read_lines "input.txt"

let stones = List.map int_of_string (String.split_on_char ' ' (List.hd file_lines))

let update_stone stone = if stone = 0 
    then [1]
    else let str = string_of_int stone in let len = String.length str in
        if len mod 2 = 0
        then [int_of_string (String.sub str 0 (len/2)); int_of_string (String.sub str (len/2) (len/2))]
        else [stone*2024]

let rec update_all stones n = if n = 0 then stones
    else let new_stones = List.fold_left ( @ ) [] (List.map update_stone stones) in
        update_all new_stones (n - 1)

let all_stones = update_all stones 25

let () = Printf.printf "Part 1: %d\n" (List.length all_stones)


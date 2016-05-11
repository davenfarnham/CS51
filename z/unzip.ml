open Core.Std
open Batteries
open Huffman
open Freq

exception CmdLineError

(*** read in bytes, decode to words (unzip) ***)

(* convert a string to its bitwise representation based on characters; "abcd" -> 1633837924 *)
let str_to_int s = 
  let length = String.length s in 
    let rec loop count total shift = 
      if count = length then total
      else (let byte = String.get s count in
              let c = (int_of_char (byte)) lsl shift in 
		loop (count + 1) (total + c) (shift + 8)) in
    loop 0 0 0
;;

(* take a string and turn it into a list of ints, which is its binary representation *)
let bit_to_str (s: string) : (int list) =
  print_string "Length of line "; print_int (String.length s); print_string "\n";
  let total = String.length s in 
    let chunk loc str =
      let length = int_of_char (String.get str loc) in print_string "Bits used in string "; print_int length; print_string "\n"; 
	let int_rep = str_to_int (String.sub str 0 4) in
          let rec inner i count = 
            if count = length then []
	    else (let check = 1 land i in 
		    check :: (inner (i lsr 1) (count + 1))) in 
	  inner int_rep 0 in  
      let rec outer t s' =
        if t >= total then []
        else (let c' = (chunk 4 (String.sub s' t 5)) in 
	       (outer (t + 5) s') @ c') in
      outer 0 s
;;

module Escaped = Map.Make(String)

(* main function *)
let unzip =

  let escaped = ref Escaped.empty in
    (escaped := Escaped.add "r" '\r' !escaped);
    (escaped := Escaped.add "n" '\n' !escaped);
    (escaped := Escaped.add "t" '\t' !escaped);
    (escaped := Escaped.add "b" '\b' !escaped);
    (escaped := Escaped.add "\'" '\'' !escaped);
    (escaped := Escaped.add "\"" '\"' !escaped); 

  (* check cmd line args *)
  let frequencies = ref [] in
    let args_len = (Array.length Sys.argv) in
      let (i, o) = (match args_len with
                    | 4 -> (frequencies := (freq_table Sys.argv.(1))); (Sys.argv.(2), Sys.argv.(3))
                    | _ -> raise CmdLineError) in

  (* create encodings *)
  let (_, decoding) = encode !frequencies in

  (* unzip *)
  let f = open_in i in
    let f' = open_out o in 
      try
        while true do
          let s = input_line f in
            let l = List.rev (bit_to_str s) in
              let s' = search decoding l in
                let rec loop l' =
                  (match l' with
                   | [] -> ()
                   | hd :: tl -> let r = Str.regexp "\\\\[a-z\'\"]" in
				   (if Str.string_match r hd 0 then (output_char f' (Escaped.find (String.sub hd 1 1) !escaped))
				   else output_string f' hd); loop tl) in
                loop s'
        done
      with End_of_file -> close_in f; close_out f'

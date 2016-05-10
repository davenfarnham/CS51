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
  let total = String.length s in 
    let chunk loc str =
      let length = int_of_char (String.get str loc) in 
        let int_rep = str_to_int (String.sub str 0 4) in
          let rec inner i count = 
            if count = length then []
	    else (let check = 1 land i in 
		    check :: (inner (i lsr 1) (count + 1))) in 
	  inner int_rep 0 in  
      let rec outer t s' =
        if t >= total then []
        else (let c' = (chunk 4 (String.sub s' t 5)) in 
	       c' @ (outer (t + 5) s')) in
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
                   | hd :: tl -> print_string hd; loop tl) in
(*
(match (loop tl) with
				  | () ->  print_string hd)) in

let r = Str.regexp "\\\\[a-z]" in (print_string hd);
				   (if Str.string_match r hd 0 then (print_string (String.sub hd 1 1); let c = Escaped.find (String.sub hd 1 1) !escaped in (output_char f' c))
				   else output_string f' hd); loop tl) in
  
*)
                loop s'
        done
      with End_of_file -> close_in f; close_out f'

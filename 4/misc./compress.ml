open Core.Std
open Batteries
open Huffman
open Scanf

exception Error

(*** codes -> writing out bytes (zip) ***)

(* i: int; s: string; l: length; u: current used bits *)
let rec bit_helper (i: int) (s: string) (l: int) (u: int) : (int * int)= 
  if (l = (String.length s)) then (i, u + l) 
  else (let v = (int_of_char s.[l]) - (int_of_char '0') in
	 let shifted = i lsl 1 in
	   let added = shifted lor v in
	     bit_helper added s (l + 1) u)
;;

let _ = assert (bit_helper 0 "101" 0 0 = (5, 3))           
let _ = assert (bit_helper 0 "11101" 0 0 = (29, 5)) 					


(* pass in string to compress *)
let rec bit_manip (s: string) (size: int) (blst: (int * int) list) (l: int) m : ((int * int) list) =
  if l = String.length s then blst 
  else (if (Encoding.mem (Char.escaped s.[l]) m) then (let value = (Encoding.find (Char.escaped s.[l]) m) in
                                            if (((String.length value) + size) > 32) then (match bit_helper 0 value 0 0 with
							                                   | (num, space) -> bit_manip s space ((num, space) :: blst) (l + 1) m)
			                    else (match blst with
						  | [] -> raise Error
						  | (hd, hd') :: tl -> let (num, space) = bit_helper hd value 0 hd' in
								         bit_manip s space ((num, space) :: tl) (l + 1) m)) 
	else raise Error)
;;

let _ = 
  let fs = [(45, ["a"]); (13, ["b"]); ( 12, ["c"]); (16, ["d"]); (9, ["e"]); (5, ["f"])] in
    let (encoding, _) = encode fs in
      assert (bit_manip "fabcdef" 0 [(0, 0)] 0 encoding = [(3237852, 22)]);
      assert (bit_manip "abcdefabcdefabcdef" 0 [(0, 0)] 0 encoding = [(3237852, 22); (1509365373, 32)])


let rec print_list l f = 
  match l with
  | [] -> ()
  | (i, i') :: tl -> (IO.write_i32 f i); (IO.write_byte f i'); (print_list tl f)
;;

(*** read in bytes, decode to words (unzip) ***)

let rec check_str s count = 
  if count = String.length s then ()
  else let c = String.get s count in
	 print_int (Char.code c); print_string "\n"; check_str s (count + 1)
;;

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

let _ = 
  let fs = [(45, ["a"]); (13, ["b"]); ( 12, ["c"]); (16, ["d"]); (9, ["e"]); (4, ["f"]); (1, [(String.escaped "\n")])] in
    let (encoding, decoding) = encode fs in

      (* zip *)
      let f = open_in "text" in 
 	let f' = open_out "out" in
	  try
	    while true do            
              let s = (input_line f) ^ "\n" in
	        let lst = bit_manip s 0 [(0, 0)] 0 encoding in
                  print_list lst f' 
	    done
	  with End_of_file -> close_in f; close_out f';

      (* unzip *)
      let f = open_in "out" in 
        try 
          while true do
            let s = input_line f in
              let l = List.rev (bit_to_str s) in
	        let s' = search decoding l in 
                  let rec loop l' = 
	            match l' with
	            | [] -> print_string "\n"
                    | hd :: tl -> if hd = "\\n" then print_string "\n" else print_string hd; loop tl in
                  loop s';
	  done
	with End_of_file -> close_in f
;;

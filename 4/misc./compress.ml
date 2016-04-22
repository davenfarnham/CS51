open Core.Std
open Huffman

exception Error

(* change ints dealing with position to char *)

(* i: int; s: string; l: length; u: current used bits *)
let rec bit_helper (i: int) (s: string) (l: int) (u: int) : (int * int)= 
  if (l = (String.length s)) then (i, u + l) 
  else (let v = (int_of_char s.[l]) - (int_of_char '0') in
	 let shifted = i lsl 1 in
	   let added = shifted lor v in
	     bit_helper added s (l + 1) u)
;;

let _ = assert (bit_helper 0 "101" 0 0 = (5, 3))           
					

(* pass in string to compress *)
let rec bit_manip (s: string) (size: int) (blst: (int * int) list) (l: int) m : ((int * int) list) =
  if l = String.length s then blst 
  else (if (Encoding.mem (Char.to_string s.[l]) m) then (let value = (Encoding.find (Char.to_string s.[l]) m) in
                                            if (((String.length value) + size) > 32) then (match bit_helper 0 value 0 0 with
							                                   | (num, space) -> bit_manip s space ((num, space) :: blst) (l + 1) m)
			                    else (match blst with
						  | [] -> raise Error
						  | (hd, hd') :: tl -> let (num, space) = bit_helper hd value 0 size in
								         bit_manip s space ((num, space) :: tl) (l + 1) m)) 
	else raise Error)
;;

let rec print_list l = 
  match l with
  | [] -> ()
  | (i, i') :: tl -> print_int i; print_int i'; print_string "\n"; print_list tl
;;

let _ = 
  let fs = [(45, ["a"]); (13, ["b"]); ( 12, ["c"]); (16, ["d"]); (9, ["e"]); (5, ["f"])] in
    let (encoding, decoding) = encode fs in
      let lst = bit_manip "abc" 0 [(0, 0)] 0 encoding in
        print_list lst

(*
      let f = open_in "text" in
        let l = input_line f in
          print_string l
*)
;;

open Core.Std
open Batteries
open Huffman
open Freq

exception Error
exception CmdLineError

(*** codes -> writing out bytes (zip) ***)

(* i: int; s: string; l: length; u: current used bits *)
let rec bit_helper (i: int) (s: string) (l: int) (u: int) : (int * int) = 
  if (l = (String.length s)) then (i, u + l) 
  else (let v = (int_of_char s.[l]) - (int_of_char '0') in
	 let shifted = i lsl 1 in
	   let added = shifted lor v in
	     bit_helper added s (l + 1) u)
;;

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

(* print out bytes to file *)
let rec print_list l f = 
  match l with
  | [] -> ()
  | (i, i') :: tl -> (IO.write_i32 f i); (IO.write_byte f i'); (print_list tl f)
;;

(* main function *)
let zip =

  (* check cmd line args *)
  let frequencies = ref [] in 
    let args_len = (Array.length Sys.argv) in 
      let name = (match args_len with
         	  | 2 -> (frequencies := (freq_table Sys.argv.(1))); Sys.argv.(1)
       	          | _ -> (print_string "/zip.native [file to zip]\n"); raise CmdLineError) in 

  (* create encodings *)
  let (encoding, _) = encode !frequencies in

  (* zip *)
  let f = open_in name in
    let f' = open_out (name ^ ".zip") in
      try
        while true do
          let s = (input_line f) ^ "\n" in (print_string s);
            let lst = bit_manip s 0 [(0, 0)] 0 encoding in
              print_list lst f'
        done
      with End_of_file -> close_in f; close_out f';

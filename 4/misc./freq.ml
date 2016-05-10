(* 
 * When run on a text file, this'll return a list of frequencies for all 
 * characters in the file. 
 *)

open Core.Std
open Batteries

(* create Hashtable and counter *)
let my_hash = Hashtbl.create 512 
let total = ref 0.

(* update hashtable *)
let update (s: string) = 
  (if Hashtbl.mem my_hash s then let value = Hashtbl.find my_hash s in
                                   Hashtbl.replace my_hash s (value + 1)
   else Hashtbl.add my_hash s 1); (total := !total +. 1.)

(* add letters -> counts to table *)
let create_freq_tabl f = 
  try
    while true do
      let s = input_line f in
        let length = String.length s in
          let rec loop s' len counter = 
            if counter = len then ()
	    else (update (Char.escaped (s'.[counter])); (loop s' len (counter + 1)))
	  in loop s length 0; (update (Char.escaped '\n'))
    done
  with End_of_file -> close_in f

(* debugging: print out frequencies straight from hashtbl *)
let print_freq () = 
  Hashtbl.iter (fun x y -> print_string (x ^ ": " ^ (String.of_float ((Float.of_int y) /. !total)) ^ "\n")) my_hash

(* debugging: print out list of frequencies *)
let rec print_lst l =
  match l with
  | [] -> ()
  | (f, s) :: tl -> print_string (s ^ ": " ^ (String.of_float f) ^ "\n"); print_lst tl

(* return a list with the frequencies *)
let tbl_to_lst () = 
  let lst = ref [] in
    Hashtbl.iter (fun x y -> (lst := (((Float.of_int y) /. !total), [x]) :: !lst)) my_hash; !lst   

let freq_table s = 
  let f = open_in s in (* not the best, since I don't think this'll close the file if there's an error *)
    create_freq_tabl f; let l = tbl_to_lst () in l

(*
 * I think this actually runs in O(n^2) - running through the list to find the smallest pairs
 * is on average (n/2), while you have to do it n times = n(n/2) = O(n^2). 
 *
 * For better running time use a heap to construct a priority queue.
 * 
 * By Daven Farnham (Feb 8th, 2016)
 *)

open Core.Std

exception UnImplemented
exception Error

(* create a mapping from letters to digits. Update as you go. *)
module Encoding = Map.Make(String);;

(* find smallest pair; return rest of list *)
let rec pair (lst : (string * int) list) : ((string * int) option * (string * int) option * (string * int) list) = 
  match lst with
  | [] -> (None, None, [])
  | hd :: [] -> (None, Some hd, [])
  | hd :: tl -> match pair tl with
	        | (None, None, []) -> (None, None, []) (* should never hit *)
		| (None, Some (rs, ri), []) -> let (_, hi) = hd in 
			       	 	         if hi > ri then (Some (rs, ri), Some hd, [])
					         else (Some hd, Some (rs, ri), [])       
		| (Some (ls, li), Some (rs, ri), lst') -> let (_, hi) = hd in 
						            if (hi < ri && hi < li) then (Some hd, Some (ls, li), (rs, ri) :: lst')
							    else if (hi < ri && hi >= li) then (Some (ls, li), Some hd, (rs, ri) :: lst')
					    	            else (Some (ls, li), Some (rs, ri), hd :: lst')				      
		| _ -> raise Error
;;

assert (pair [] = (None, None, []))
assert (pair [("a", 1); ("b", 2); ("c", 3)] = (Some ("a", 1), Some ("b", 2), [("c", 3)]))
assert (pair [("a", 1); ("c", 3); ("b", 2)] = (Some ("a", 1), Some ("b", 2), [("c", 3)]))
assert (pair [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] = 
	     (Some ("f", 5), Some ("e", 9), [("a", 45); ("b", 13); ("c", 12); ("d", 16)]))
assert (pair [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("ef", 14)] = 
	     (Some ("c", 12), Some ("b", 13), [("a", 45); ("ef", 14); ("d", 16)]))
assert (pair [("h",1);("g",1);("f",1);("e",1);("c",1);("d",1);("b",3);("a",8)] = 
	     (Some ("d", 1), Some ("c", 1), [("h",1);("g",1);("f",1);("e",1);("b",3);("a",8)]))

(* increment all strings in map by incr *)
let rec incr_map (incr : string) (s : string) (map : (string Encoding.t ref)) : unit = 
  if (Encoding.mem !map (Char.escaped s.[0])) then (match Encoding.find !map (Char.escaped s.[0]) with
						    | None -> raise Error
						    | Some v -> (map := Encoding.add !map ~key:(Char.escaped s.[0]) ~data:(incr ^ v)))
  else (map := Encoding.add !map ~key:(Char.escaped s.[0]) ~data:incr); 
  match String.length s with
  | 1 -> ()
  | _ -> incr_map incr (String.sub s ~pos:1 ~len:((String.length s) - 1)) map
;;

(* merge entries in list; incr map *)
let rec encode (map : (string Encoding.t ref)) (lst : (string * int) list) : unit = 
  match lst with
  | [] -> () (* change to print out map -> list *)
  | _ :: _ -> match pair lst with
	      | (None, None, []) -> ()
	      | (Some _, None, tl') -> encode map tl'
	      | (None, Some _, tl') -> encode map tl'
	      | (Some (ls, li), Some (rs, ri), tl') -> (incr_map "0" ls map); (incr_map "1" rs map); encode map (((ls ^ rs), (li + ri)) :: tl') 
              | _ -> ()
;;
  
(* encode a list of strings that have declared probabilities *)
let huffman (fs : (string * int) list) : (string Encoding.t ref) = 
  let e = ref (Encoding.empty) in
    (encode e fs); e
;;

(* convert map back to list *)
let map_to_list (map : (string Encoding.t ref)) : (string * string) list = 
  Encoding.fold !map ~f:(fun ~key:key ~data:value i -> (key, value) :: i) ~init:[]
;;

(* testing *)
let rec print_list (lst : (string * string) list) : unit = 
  match lst with
  | [] -> ()
  | hd :: tl -> let (s, s') = hd in 
		  (print_list tl); print_string (s ^ ": " ^ s' ^ "\n")
;;

(* testing *) (*
print_list (map_to_list (huffman ([("a",45);("b",13);("c",12);("d",16);("e",9);("f",5)])));
print_list (map_to_list (huffman ([("h",1);("g",1);("f",1);("e",1);("c",1);("d",1);("b",3);("a",8)]))) 
print_list (map_to_list (huffman ([("a",24);("b",12);("c",10);("d",8);("e",8)]))) 
print_list (map_to_list (huffman ([("a",60);("b",5);("c",30);("d",5)]))) *)

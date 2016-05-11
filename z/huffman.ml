(**** Huffman encoding using priority queue ****)

(*
 * Use a min-heap to create a binary tree. The binary heap in this implementation
 * is such that the left tree is always filled first, but will only have, at most, 
 * one more item in it than the right tree. This'll minimize the height and ensure 
 * operations like insert and take (fix) run in log(n).
 *)

type balance = Even | Odd
type tree = Empty | Leaf | Branch of balance * (float * string list) * tree * tree

exception Error
exception Get_Last_Error
exception Get_Node_Error
exception Fix_Error

(* get top node of tree *)
let get_node (t: tree) : (float * string list) =
  match t with
  | Empty | Leaf -> raise Get_Node_Error
  | Branch (_, v, _, _) -> v
;;

(* switch balance *)
let switch (b: balance) : balance = 
  match b with
  | Odd -> Even
  | _ -> Odd
;;

(* print out tree contents df traversal *)
let rec depth (t: tree) : unit =
  match t with
  | Leaf | Empty -> ()
  | Branch (_, (i, s), l, r) -> print_string ((string_of_float i) ^ "," ^ (List.fold_left (fun x y -> x ^ y) "" s) ^ "\n"); depth l; depth r
;; 

(* for debugging *)
let bal_to_string (b: balance) : string = 
  match b with
  | Odd -> "Odd"
  | Even -> "Even"
;;

(* print out tree contents bf traversal *)
let breadth (t: tree) : unit = 
  let rec loop lst = 
    match lst with
    | [] | Empty :: _ -> ()
    | Leaf :: tl -> loop tl
    | Branch (b, (i, s), l, r) :: tl -> print_string ((bal_to_string b) ^ "," ^ 
						      (string_of_float i) ^ "," ^ 
						      (List.fold_left (fun x y -> x ^ y) "" s) ^ "\n"); loop (tl @ (l :: [r])) in
  loop [t]
;;

(* insert a number into tree; should run in O(log n) *)
let rec insert ((i, s): (float * (string list))) (t: tree) : tree =
  match t with
  | Leaf | Empty -> Branch (Even, (i, s), Leaf, Leaf) 
  | Branch (Even, v, l, r) -> (match v with
			       | (i', s') -> if i < i' then Branch (Odd, (i, s), insert (i', s') l, r)
			        	     else Branch (Odd, v, insert (i, s) l, r))
  | Branch (Odd, v, l, r) -> (match v with
                              | (i', s') -> if i < i' then Branch (Even, (i, s), l, insert (i', s') r)
			       		    else Branch (Even, v, l, insert (i, s) r))
;;		

(* take last element from a tree; should run in O(log n) *)
let rec get_last (t: tree) : ((float * string list) * tree) option =
  match t with
  | Empty | Leaf -> None
  | Branch (_, v, Leaf, Leaf) -> (Some (v, Leaf))
  | Branch (Odd, v, l, r) -> (match get_last l with
			      | None -> None
			      | Some (v', Leaf) -> (Some (v', Branch (Even, v, Leaf, r)))
			      | Some (v', l') -> (Some (v', Branch (Even, v, l', r))))
  | Branch (Even, v, l, r) -> (match get_last r with
			       | None -> None
			       | Some (v', Leaf) -> (Some (v', Branch (Odd, v, l, Leaf)))
			       | Some (v', r') -> (Some (v', Branch (Odd, v, l, r'))))
;;

(* fix a tree so that it's a min heap again *)
let rec fix (t: tree) : tree =
  match t with
  | Leaf | Empty -> raise Fix_Error
  | Branch (_, _, Leaf, Leaf) -> t
  
  (* balance used to be only Odd -> changed it to deal with equal probabilities *)
  | Branch (Odd, (num, lst), l, Leaf) -> (match l with
				          | Leaf | Empty -> raise Fix_Error
			                  | Branch (b, (num', lst'), l', r') -> if num' < num then Branch(Odd, (num', lst'), Branch (b, (num, lst), l', r'), Leaf)
									        else Branch (Odd, (num, lst), fix l, Leaf))

  (* this is somewhat complicated since insert doesn't guarantee l < r *)
  | Branch (bal, (num, lst), l, r) -> let (i, _) = get_node l in
				        let (i', _) = get_node r in 
					  let b = if i < i' || (i = i' && bal = Odd) then Odd else Even in
					    (match b with
					     | Odd -> (match l with
				       		       | Leaf | Empty -> raise Fix_Error
			               		       | Branch (b', (num', lst'), l', r') -> 
							  if num' < num then Branch(bal, (num', lst'), fix (Branch (b', (num, lst), l', r')), r)
							  else Branch (bal, (num, lst), fix l, r))
					     | Even -> (match r with
							| Leaf | Empty -> raise Fix_Error
							| Branch (b', (num', lst'), l', r') -> 
							   if num' < num then Branch(bal, (num', lst'), l, fix (Branch (b', (num, lst), l', r')))
					    	       	   else Branch (bal, (num, lst), l, fix r)))
;;

(* take element with lowest priority and fix tree *)
let take (t: tree) : ((float * string list) * tree) option = 
  match t with
  | Empty | Leaf -> None
  | Branch (b, v, l, r) -> let min = v in
			     (match b with 
			      | Odd -> (match get_last l with
					| None -> (Some (v, Empty))			
					| Some (v', l') -> (Some (min, fix (Branch (Even, v', l', r)))))
			      | Even -> (match get_last r with
					 | None -> (Some (v, Empty))
					 | Some (v', r') -> (Some (min, fix (Branch (Odd, v', l, r'))))))
;;

(* search through huffman tree *)
let search (t: tree) (lst: int list) : (string list) = 
  let rec loop t' lst' =
    match t' with
    | Leaf -> raise Error  
    | Branch(_, v, Leaf, Leaf) -> let (_, s) = v in s @ (loop t lst') 
    | _ -> (match lst' with
    	    | [] -> []
      	    | 1 :: tl  -> (match t' with
	          	   | Branch(_, _, _, r) -> loop r tl
	             	   | _ -> raise Error)
      	    | 0 :: tl -> (match t' with
	      	     	  | Branch(_, _, l, _) -> loop l tl
	             	  | _ -> raise Error)
            | _ -> raise Error) in
  loop t lst
;;

(* complexity goes downhill starting... now *)

(*
 * Whereas the above only dealt with trees, here I use ocaml's Map module
 * to encode letters to their binary strings. For example, the map might have
 * something like this:
 *
 *		"a" -> "10101"
 *		"b" -> "101"
 *		...
 *
 * Map creates a balanced binary tree, again ensuring O(log n) running time for insert and take.  
 * 
 *
 * 		[("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]
 *
 *
 *					["abcdef"]
 *				      /		   \
 *				   ["a"]          ["bcdef"]
 *					       /	     \
 *					   ["bc"]	    ["def"]
 *					  /      \	   /       \
 *				       ["c"]     ["b"]  ["ef"]     ["d"]
 *						       /      \
 *						     ["f"]    ["e"]
 *
 * (1) First, take a list of tuples containing a (frequency * string list) and add them to the tree.
 * This tree will use the above code, effectively creating a huffman tree. 
 *
 * (2) In create_encoding, everytime I take something from the huffman tree O(log n) I have to add either a "1" or a "0" to that 
 * string's association in the map. Finding the string in the map will be O(log n) while adding the "1" or "0" will 
 * depend on the length of the string. In total: 
 *
 *			- taking from the tree: 		n * log(n) 
 *			- adding to map: 			n * log(n)
 *			- creating huffman tree			~ (n * log(n))	
 * 			- total asymptotic:			n * log(n)
 * 
 *
 *)

module Encoding = Map.Make(String)

(* new module to pass into Map.Make for ordering string lists *)
module ListString : Map.OrderedType with type t = (string list) = 
struct 
  type t = string list

  let rec compare l r =
    match (l, r) with
    | ([], []) -> 0
    | (_ :: _, []) -> 1
    | ([], _ :: _) -> -1 
    | (hd :: tl, hd' :: tl') -> let c = String.compare hd hd' in 
				  if c = 0 then compare tl tl' else c
end

module Huffman = Map.Make(ListString)

(* create tree from list; O(nlog(n)) *)
let rec add_to_tree lst = 
  match lst with
  | [] -> Empty
  | hd :: tl -> insert hd (add_to_tree tl)
;;

(* add value to all chars in list *)
let rec add_to_map l m path = 
  match l with
  | [] -> ()
  | hd :: tl -> if (Encoding.mem hd !m) then (let value = (Encoding.find hd !m) in
					        (m := Encoding.add hd (path ^ value) !m); add_to_map tl m path)
	        else ((m := Encoding.add hd path !m); add_to_map tl m path)
;;

(* create a mapping for the huffman codes *)
let create_encoding t = 
  (* map encoding *)
  let m = ref (Encoding.empty) in
  (* huffman tree *)
  let hf = ref (Huffman.empty) in 
    let rec loop t' l = 
      match t' with
      | Leaf | Empty -> l
      | _ -> (match take t' with
	      | Some ((i, s), t'') -> (match t'' with 
				       | Leaf | Empty -> l
				       | _ -> (match take t'' with
			                       | Some ((i', s'), tr) -> add_to_map s m "0"; 
									add_to_map s' m "1";
									if Huffman.mem s !hf then () 
									else (hf := Huffman.add s (Branch (Even, (i, s), Leaf, Leaf)) !hf);
									if Huffman.mem s' !hf then ()
									else (hf := Huffman.add s' (Branch (Even, (i', s'), Leaf, Leaf)) !hf);
									let l = Huffman.find s !hf in
									  let r = Huffman.find s' !hf in
									    (hf := Huffman.add (s @ s') (Branch (Even, (i +. i', s @ s'), l, r)) !hf);
									      let tr' = (insert (i +. i', s @ s') tr) in  
									        loop tr' (s @ s')
				               | _ -> l)) 
	      | _ -> l) in
  let index = loop t [] in (!m, (Huffman.find index !hf))
;;

(* return tuple (tree codes -> letters, mapping letters -> codes) *)
let encode l = 
  let t = add_to_tree l in
    let (tr, code) = create_encoding t in
      (tr, code)
;;

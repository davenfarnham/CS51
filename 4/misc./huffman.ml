(* Huffman encoding using priority queue *)

type balance = Even | Odd
type tree = Empty | Leaf | Branch of balance * (int * string list) * tree * tree

exception Error
exception Get_Last_Error
exception Get_Node_Error
exception Fix_Error

(* get top node of tree *)
let get_node (t: tree) : (int * string list) =
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

(* insert a number into tree; should run in O(log n) *)
let rec insert ((i, s): (int * (string list))) (t: tree) : tree =
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
let rec get_last (t: tree) : ((int * string list) * tree) option =
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
			       | Some (v', r') -> (Some (v', Branch (Even, v, l, r'))))
;;

(* fix a tree so that it's a min heap again *)
let rec fix (t: tree) : tree =
  match t with
  | Leaf | Empty -> raise Fix_Error
  | Branch (_, _, Leaf, Leaf) -> t
  | Branch (Odd, (num, lst), l, Leaf) -> (match l with
				          | Leaf | Empty -> raise Fix_Error
			                  | Branch (b, (num', lst'), l', r') -> if num' < num then Branch(Odd, (num', lst'), Branch (b, (num, lst), l', r'), Leaf)
									        else Branch (Odd, (num, lst), fix l, Leaf))
  (* this is somewhat complicated since insert doesn't guarantee l < r *)
  | Branch (bal, (num, lst), l, r) -> let (i, _) = get_node l in
				        let (i', _) = get_node r in 
					  let b = if i < i' then Odd else Even in
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
let take (t: tree) : ((int * string list) * tree) option = 
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

(* complexity goes downhill starting... now *)

module Encoding = Map.Make(String)

(* create tree from list *)
let rec add_to_tree lst = 
  match lst with
  | [] -> Empty
  | hd :: tl -> insert hd (add_to_tree tl)
;;

(* list of tuples -> list of trees *)
let rec treelist lst = 
  match lst with
  | [] -> []
  | hd :: tl -> (Branch (Even, hd, Leaf, Leaf)) :: treelist tl
;;

(* print out tree contents df traversal *)
let rec depth (t: tree) : unit =
  match t with
  | Leaf | Empty -> ()
  | Branch (_, (i, s), l, r) -> print_string ((string_of_int i) ^ "," ^ (List.fold_left (fun x y -> x ^ y) "" s) ^ "\n"); depth l; depth r
;; 

(* print out tree contents bf traversal *)
let breadth (t: tree) : unit = 
  let rec loop lst = 
    match lst with
    | [] | Empty :: _ -> ()
    | Leaf :: tl -> loop tl
    | Branch (_, (i, s), l, r) :: tl -> print_string ((string_of_int i) ^ "," ^ (List.fold_left (fun x y -> x ^ y) "" s) ^ "\n"); loop (tl @ (l :: [r])) in
  loop [t]
;;

(* add value to all chars in list *)
let rec add_to_map l m path = 
  match l with
  | [] -> ()
  | hd :: tl -> if (Encoding.mem hd !m) then (let value = (Encoding.find hd !m) in
					        (m := Encoding.add hd (path ^ value) !m); add_to_map tl m path)
	        else ((m := Encoding.add hd path !m); add_to_map tl m path)
;;

(* find a tree in a treelist *)
let rec list_find l a =
  match l with
  | [] -> raise Error
  | Branch(_, v, _, _) as br :: tl -> if v = a then (br, tl) else
				      (match list_find tl a with
				       | (v', tl') -> (v', br :: tl'))
  | _ -> raise Error
;;

(* create a mapping for the huffman codes *)
let create_encoding t lst = 
  let m = ref (Encoding.empty) in
    let rec loop t' tlst = 
      match t' with
      | Leaf | Empty -> tlst
      | _ -> (match take t' with
	      | Some ((i, s), t'') -> (match t'' with 
				       | Leaf | Empty -> tlst
				       | _ -> (match take t'' with
			                       | Some ((i', s'), tr) -> add_to_map s m "0"; (* dependent on length of s *)
									add_to_map s' m "1";
									let (l, tlst') = list_find tlst (i, s) in (* O(n) *)
									  let (r, tlst'') = list_find tlst' (i', s') in 
									    let combo = (Branch (Even, (i + i', s @ s'), l, r)) in
									      (loop (insert (i + i', s @ s') tr) (combo :: tlst''))
				               | _ -> tlst)) 
	      | _ -> tlst) in
  let decode = loop t lst in (!m, decode)
;;

(* return tuple (tree codes -> letters, mapping letters -> codes) *)
let encode l = 
  let t = add_to_tree l in
    let tlst = treelist l in
      let (tr, code) = create_encoding t tlst in
        match code with
        | [] -> raise Error
	| hd :: _ -> (tr, hd)
;;

open Core.Std

(* given a tree, perform (1) depth first then (2) breadth first searches *)

(* define a tree *)
type tree = Leaf | Branch of tree * int * tree

let rec depth (t: tree) : unit = 
  match t with
  | Leaf -> ()
  | Branch (l, i, r) -> print_int i; depth l; depth r
;;

(*
 *			5
 *		      /   \
 * 		     1     6		-> 5, 1, 4, 3, 6, 7, 9
 *		    / \   /  \
 *		   4   3 7    9
 *)

(* 5, 1, 4, 3, 6, 7, 9 *)
let _ = 
  let t = (Branch (Branch (Branch (Leaf, 4, Leaf), 1, Branch (Leaf, 3, Leaf)), 5, 
	           Branch (Branch (Leaf, 7, Leaf), 6, Branch (Leaf, 9, Leaf)))) in
    depth t; print_string "\n"
;;

let breadth (t: tree) : unit = 
  let rec breadth_help (wl: tree list) : unit = 
    match wl with
    | [] -> ()
    | Leaf :: rest -> breadth_help rest
    | Branch (l, v, r) :: rest -> print_int v; breadth_help (rest @ [l;r]) in
  breadth_help [t]; print_string "\n"
;;

(* 5, 1, 6, 4, 3, 7, 9 *)
let _ = 
  let t = (Branch (Branch (Branch (Leaf, 4, Leaf), 1, Branch (Leaf, 3, Leaf)), 5, 
	           Branch (Branch (Leaf, 7, Leaf), 6, Branch (Leaf, 9, Leaf)))) in
    breadth t
;;

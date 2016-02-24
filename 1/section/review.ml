open Core.Std
exception Unimplemented

(* LISTS *)

(* We're going to introduce some simple lists.
 * To start, make 'l1' be a list of the following numbers in order: 3, 4, 5 *)
let l1 = [3;4;5];;

(* Try to make l2 be a list of 4, followed by "Henry". Does it work?
 * Why or why not? If it doesn't work, just make l2 be the empty list. *)
let l2 = [];;

(* Now we're going to do some basic matching on lists.
 *
 * It's fun to get the hang of 'match'. In some ways, a 'match' can be thought
 * of as analogous to an 'if' statement or a 'switch' statement, in that you're
 * choosing which branch of code to follow, based on the value of a variable.
 *
 * Here's a simple example. It takes a list and returns true if the list
 * is empty and false if the list is not empty.
 *
 * (Tip: Notice the use of _, the wildcard operator. If we had said "hd :: tl"
 * in the second match case, we would have unnecessarily created variables
 * called "hd" and "tl", which we would never use.)
 *)
let list_is_empty (lst : int list) : bool =
  match lst with
  | [] -> true
  | _ :: _ -> false

(* Now, see if you can tackle the following functions... *)

(* Return the head of a list, or None if empty. *)
let head (x : int list) : int option =
  match x with
  | [] -> None
  | hd :: _ -> Some hd
;;

(* Return the tail of a list, or None if empty. *)
let tail (x : int list) : int list option =
  match x with
  | [] -> None
  | _ :: tl -> Some tl
;;

(* Square all the elements of a list. *)
let rec square_all (a : int list) : int list =
  match a with
  | [] -> []
  | hd :: tl -> Float.to_int ((Float.of_int hd) ** 2.) :: square_all tl (* '**' only works with floats *)
;;

(* Retain only even integers (notice the "let rec"). *)
let rec filter_even (l : int list) : int list =
  match l with
  | [] -> []
  | hd :: tl -> (if hd % 2 = 0 then hd :: filter_even tl
		 else filter_even tl)
;;

(* Return the last int of an int list, or None if empty.
 * You may need an extra match case. *)
let rec last_number (x : int list) : int option =
  match x with
  | [] -> None
  | hd :: [] -> Some hd
  | _ :: tl -> last_number tl
;;

(* Return the max of a list or None if the list is empty. *)
(* Note: Might be good to walk through this in English before syntactifying *)
let rec max_of_list (x : int list) : int option =
  match x with
  | [] -> None
  | hd :: tl -> match max_of_list tl with
		| None -> Some hd
		| Some v' -> if hd > v' then Some hd else Some v'
;;

assert (max_of_list [] = None)
assert (max_of_list [1] = Some 1)
assert (max_of_list [1;2;3;4] = Some 4)
assert (max_of_list [5;4;3;2;1] = Some 5)

(* Compute the dot product of two lists.
 * Write sum, and then use sum, zip, and prods to write dotproduct *)

(* Returns the sum of the elements of a list *)
let sum (l : int list) : int =
  List.fold_left ~f:(fun x y -> x + y) ~init:0 l
;;

(* frustration with Core's List.fold_left syntax *)
let rec fold_left f i l = 
  match l with
  | [] -> i
  | hd :: tl -> (f hd (fold_left f i tl))
;;

(* multiply elements of tuple *)
let prods (l : (int * int) list) : int list =
  fold_left (fun x y -> let (l', r') = x in (l' * r') :: y) [] l
;;

exception ZipError

let rec zip (x : int list) (y : int list) : ((int * int) list) option =
  match (x, y) with
  | ([], []) -> None
  | (_, []) -> raise ZipError (* can't zip lists of unequal length *)
  | ([], _) -> raise ZipError
  | (hdx :: tlx, hdy :: tly) -> (match zip tlx tly with
				 | None -> Some [(hdx, hdy)]
			 	 | Some l -> Some ((hdx, hdy) :: l))
;;

assert (zip [] [] = None)
assert (zip [1;2] [3;4] = Some [(1, 3); (2, 4)])

(* Even without looking at the code for the functions, carefully
 * looking at the type signatures for zip, prods, and sum should
 * give a good idea of how you might combine these functions to
 * implement dotproduct. *)

let dotproduct (a : int list) (b : int list) : int option =
  match zip a b with
  | None -> None
  | Some l -> Some (sum (prods l))
;;

assert (dotproduct [] [] = None)
assert (dotproduct [1;2;3] [4;5;6] = Some 32)

(* Zip three lists. Return None if different lengths. *)
(* [1;2] [3;4] [5;6] -> Some [(1, 3, 5); (2, 4, 6)] *)
let rec threezip (a : int list) (b : int list) (c : int list) :
		 ((int * int * int) list) option =
  match (a, b, c) with
  | ([], [], []) | ([], _, _) | (_, [], _) | (_, _, []) -> None
  | (hda :: [], hdb :: [], hdc :: []) -> Some [(hda, hdb, hdc)] (* to avoid having to use List.length *)
  | (hda :: tla, hdb :: tlb, hdc :: tlc) -> (match threezip tla tlb tlc with
					     | None -> None
					     | Some l -> Some ((hda, hdb, hdc) :: l))
;;

assert (threezip [] [] [] = None) 
assert (threezip [1;2] [3;4] [5;6;7] = None) 
assert (threezip [1] [3] [5] = Some [(1, 3, 5)]) 
assert (threezip [1;2] [3;4] [5;6] = Some [(1, 3, 5); (2, 4, 6)]) 

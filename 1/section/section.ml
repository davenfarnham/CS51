(* CS51 Section 0: Intro to ML
 *
 * Exercises: The purpose of these exercises is to help you start
 * getting comfortable with Ocaml.  The focus is on pattern matching,
 * list operations, and a bit of basic arithmetic.
 *
 * A goal throughout the semester will be writing code that's clear,
 * concise, and beautiful -- not just correct.  Try to make your
 * solutions as simple as possible.  Once you have a version that works,
 * look for ways to simplify it. *)

open Core.Std

(* MATH *)

(* Make it so that that x equals 42, by adding 22 to 20 *)
let x = 22 + 20;;

(* Make it so that x1 equals 42.0, by adding 2 numbers. *)
let x1 = 20. +. 22.;;

(* Write a function that takes a number and returns
 * the difference between that number and 42.
 * Eg, if 'num' is 50, the result should be 8.
 * If 'num' is 30, the result should be -12 *)
let difference_between_x_and_42 (num : int) : int = 
  num - 42
;;

(* One more simple arithmetic example...
es * Write a function that returns the volume of a cylinder
 * with height h and radius r. *)
let volume_cylinder (h:float) (r:float) : float = 
  3.14 *. r ** 2. *. h
;;


(* Write a function that returns whether or not a number is even. *)
let even (x: int) : bool =
  x % 2 = 0
;;

(* Can you write odd using the already-implemented even? *)
let odd (x: int) : bool =
   not (even x)
;;

(* STRINGS *)

(* Write a function that takes a string and appends
 * ", and that is why I love CS51!" to the end of it. *)
let cs51_loveifier (input : string) : string =
  input ^ " and that is why I love CS51!"
;;

(* OCaml comes pre-packaged with a standard library that includes
 * a lot of utility functions.
 * For instance, check out the String module
 * (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html).
 *
 * If you want to use one of these functions in your code, you can do so
 * with String.function_name(arguments).
 *
 * Now... write a function that takes a String, and returns whether
 * or not that String is more than 10 characters long. *)
let is_more_than_10_characters_long (str : string) : bool =
  String.length str > 10
;;

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
  | hd :: tl -> (hd * hd) :: square_all tl
;;

(* Retain only even integers (notice the "let rec"). *)
let rec filter_even (l : int list) : int list =
  match l with
  | [] -> []
  | hd :: tl -> if even hd then hd :: filter_even tl
		else filter_even tl
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
  | hd :: [] -> Some hd
  | hd :: tl -> let (Some number) = max_of_list tl in if number > hd then Some number else Some hd
;;

(* staff solution *)
let rec max_of_list_ (x : int list) : int option =
  match x with 
  | [] -> None
  | hd :: tl -> (match max_of_list_ tl with
		 | None -> Some hd
		 | Some x -> Some (if x > hd then x else hd))
;;

(* tail recursion is when there is nothing to do after the functon call
 * except return the value. "x * function(y)" is not tail recursive, while
 * function(y, x * z) could be. *)
let rec max_of_list' (x : int list) : int option =
  match x with 
  | [] -> None
  | hd :: [] -> Some hd 
  | hd :: hd1 :: tl -> max_of_list' ((if hd > hd1 then hd else hd1) :: tl) 
;;

(* max_of_list' [1;5;2;3];; -> max_of_list' [5;2;3] -> max_of_list' [5;3] -> max_of_list' -> [5] -> Some 5 
 * this just returns, without doing anything, all the way up the call stack *)

(* Compute the dot product of two lists.
 * Write sum, and then use sum, zip, and prods to write dotproduct *)

(* Returns the sum of the elements of a list *)
let rec sum (l : int list) : int =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

let rec prods (l : (int * int) list) : int list =
  match l with
  | [] -> []
  | (x, y) :: tl -> (x * y) :: (prods tl)
;;

let rec zip (x : int list) (y : int list) : ((int * int) list) option =
  match (x, y) with
  | ([], []) -> Some []
  | (xhd :: xtl, yhd :: ytl) ->
    (match zip xtl ytl with
    | None -> None
    | Some ztl -> Some ((xhd, yhd) :: ztl))
  | (_, _) -> None
;;

(* Even without looking at the code for the functions, carefully
 * looking at the type signatures for zip, prods, and sum should
 * give a good idea of how you might combine these functions to
 * implement dotproduct. *)

let dotproduct (a : int list) (b : int list) : int option =
  match zip (a) (b) with
  | None -> None
  | Some x -> Some (sum(prods(x)))
;;

(* let Some (x) = (zip (a) (b)) in Some (sum(prods x)) *) (* incomplete matching *) 


(* Zip three lists. Return None if different lengths. *)
(* [1;2] [3;4] [5;6] -> Some [(1, 3, 5); (2, 4, 6)] *)
let rec threezip (a : int list) (b : int list) (c : int list) :
  ((int * int * int) list) option =
  match (a, b, c) with
  | ([], [], []) -> Some []
  | (_, _, []) -> None
  | (_, [], _) -> None
  | ([], _, _) -> None
  | (hd1 :: tl1, hd2 :: tl2, hd3 :: tl3) -> Some (match threezip tl1 tl2 tl3 with
					          | None -> []
					          | Some x -> (hd1, hd2, hd3) :: x) 
;;

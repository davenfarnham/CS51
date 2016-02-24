(* CS51 Section 2 *)
(* Goal for today: more practice with Ocaml. In particular, we'll be
   starting to see the power of higher order functions.

  Agenda:
   * Algebraic Data Types
   * Higher-order functions and polymorphism
   * Map and fold
*)

open Core.Std

(* Algebraic Data Types *)

(* Exercise 1 *)
(* Look at the following type definition. *)
(* Here, you're basically defining your own types. The bars in between mean I can choose a 'color'
 * of type Red or Yellow or Blue or Green or Crimson or Other (which i'll then define with a string).  *)
type color = Red | Yellow | Blue | Green | Crimson | Other of string
type favorite = Color of color | Movie of string | Tvshow of string |
    Number of float | Letter of char

(* We've defined some sample lists of favorite movies/colors/etc.
 * You can think of each of these lists as representing someone's
 * input describing their favorite things.
 * You may want to use these for testing your functions later.*)

let a : favorite list = [Movie "A Beautiful Mind"; Color Blue;
                         Tvshow "The Simpsons"; Color Crimson];;
let b : favorite list = [Number 1.0; Number 2.0; Number 5.0;
                         Number 14.0; Number 42.0];;
let c : favorite list = [Movie "Love Story"; Tvshow "On Harvard Time";
                         Letter 'H'; Color Crimson];;
let d : favorite list = [Tvshow "Lost"; Number 3.14];;

let students = [a; b; c; d];;

(* 1a. Define a value of type favorite list for someone whose
 * favorite color is chartreuse and whose favorite number is 5. *)
(* you have a color, but you can't use any of the predefined colors above, so you
 * have to say Other "some string" *)
let prob1a : favorite list = [Color (Other "Chartreuse"); Number 5.0]
;;

(* 1b. Write a function that takes a value of type favorite list (like the
 * ones above) and returns the title of this person's favorite movie, or
 * None if a favorite movie isn't given. If multiple movies are listed,
 * return the first. What return type does this function have? *)
let rec favmovie (lst: favorite list) : string option =
  match lst with
  | [] -> None
  | hd :: tl -> match hd with
		| (Movie m) -> Some m
                | _ -> favmovie tl
;;

(* better, less match statements *)
let rec favmovie_ (lst: favorite list) : string option = 
  match lst with
  | [] -> None
  | Movie m :: _ -> Some m
  | _ :: tl -> favmovie_ tl
;;

(* 1c. Write a function that takes a value of type favorite list and
 * returns true if and only if this person has listed Crimson as a
 * favorite color. *)
let rec harvardpride (lst: favorite list) : bool =
  match lst with
  | [] -> false
  | Color c :: tl -> if c =  Crimson then true else harvardpride tl
  | _ :: tl -> harvardpride tl
;;

(* match with Crimson instead of variable c *)
let rec harvardpride_ (lst : favorite list) : bool =
  match lst with
  | [] -> false
  | (Color Crimson) :: tl -> true
  | _ :: tl -> harvardpride_ tl
;;

(* 1d. Write a function that takes a list of favorite lists and returns all
   of the favorite lists that listed Crimson as a favorite. *)
let rec harvardfilter (lst: favorite list list) : favorite list list =
  match lst with
  | [] -> []
  | hd :: tl -> (if harvardpride hd then hd :: (harvardfilter tl) else (harvardfilter tl)) 
;;

(* bonus: is there a more concise way to write harvardfilter using higher
 * order functions? You may define your own function, or use one in the
 * OCaml library! *)
let harvardfilter' =
  List.filter ~f:(fun x -> harvardpride x)
;;

(* Exercise 2 *)
(* 2a. Define an algebraic data type representing either ints or floats *)
type realnum = Int of int | Float of float
;;

(* 2b. Define a function testing whether two realnums are equal. It
 * shouldn't matter whether they are ints or floats,
 * e.g (realequal 4 4.0) => True. *)
let realequal (a: realnum) (b: realnum) : bool =
  match (a, b) with
  | (Int i1, Int i2) -> i1 = i2
  | (Float f1, Float f2) -> f1 = f2
  | (Int i1, Float f1) -> i1 = (Float.to_int f1)
  | (Float f1, Int i1) -> i1 = (Float.to_int f1)
;;

(* Now, take a look at this data type, representing a boolean expression *)
type expr = Value of bool | Not of expr | And of expr * expr | Or of expr * expr
;;
(* 2c. Write a function that takes in an expr and returns whether it evaluates
 * to true or false. *)
let rec eval (a:expr) : bool =
  match a with
  | Value v -> v
  | Not e -> (not (eval e))
  | And (e1, e2) -> eval e1 && eval e2
  | Or (e1, e2) -> eval e1 || eval e2
;;

(* Higher-order functions and polymorphism *)
(* Exercise 3 *)

(*
let add x y = x + y -> (fun x -> (fun y -> x + y))
  (fun x -> (fun y -> x + y)) 2 3 -> (fun x -> (fun y -> x + 3))
    (fun x -> x + 3) 2 -> 2 + 3 -> 5
*)

(*
length int list -> int;;

length' float list -> int;

length_ 'a list -> int;

*)

(* 3a. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. *)
let min_option (x: int option) (y: int option) : int option =
  match (x, y) with
  | (None, None) -> None
  | (None, r) -> r
  | (l, None) -> l
  | (Some l, Some r) -> if l < r then Some l else Some r (* can also use: Some (min l r) *)
;;

let min_option_ (x: int option) (y: int option) : int option =
  match (x, y) with
  | (None, None) -> None
  | (None, r) -> r
  | (l, None) -> l
  | (Some l, Some r) -> Some (min l r) 
;;

(* 3b. Write a function to return the larger of two int options, or None
 * if both are None. If exactly one argument is None, return the other. *)
let max_option (x: int option) (y: int option) : int option =
  match (x, y) with
  | (None, None) -> None
  | (None, r) -> r
  | (l, None) -> l
  | (Some l, Some r) -> if l < r then Some r else Some l (* Some (max l r) *)
;;

(* Great! But do you see a pattern? How can we factor out similar code? *)
(* 3c. Write a higher-order function for binary operations on options.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is calc_option's function signature? *)
let calc_option (f: 'a -> 'a -> 'a) (x: 'a option) (y: 'a option) : 'a option =
  match (x, y) with
  | (None, None) -> None
  | (None, r) -> r
  | (l, None) -> l
  | (Some l, Some r) -> Some (f l r) 
;;

(* 3d. Now rewrite min_option and max_option using the higher-order function.
   The built-in functions min and max may be useful *)
let min_option_2 (x: int option) (y: int option) : int option =
  calc_option (fun x y -> (min x y))  (* min x y *) x  y
;;

let max_option_2 (x: int option) (y: int option) : int option =
  calc_option (fun x y -> (max x y)) (* max x y *) x y
;;

(* Cool! Now, can we use this in other ways? *)
(* 3e. Write a function to return the boolean AND of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)
let and_option (x:bool option) (y: bool option) : bool option =
  calc_option (fun x y -> x && y) x y
;;

(* 3f. Write a function to return the boolean OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)
let or_option (x:bool option) (y: bool option) : bool option =
  calc_option (fun x y -> x || y) x y
;;

(* Map and fold *)
(* Exercise 4 *)

let rec fold_right' f u xs =
  match xs with
  | [] -> u
  | hd :: tl -> f hd (fold_right' f u tl);;

let rec fold_right f u xs =
    match xs with
    | [] -> u (* base case *)
    | hd::tl -> f hd (fold_right f u tl);;

let rec map' f xs = 
  match xs with
  | [] -> []
  | hd :: tl -> (f hd) :: (map' f tl);;

let rec map f xs =
  match xs with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl);;

(* 4a. Implement length in terms of fold_right.
 * length lst returns the length of lst. length [] = 0. *)
let length (lst: int list) : int =
  fold_right (fun x y -> 1 + y) 0 lst  
;;

(* common mistake *)
(* fold_right (fun x y -> x + 1) 0 lst  *)
(* (f 1 (f 2 (f 3 0))) *) (* the last thing you do is 1 + 1 = 2 *)

(* Is there a way to write this without the argument? *)
let length' (int) =
  fold_right (fun x y -> 1 + y) 0
;;

(* 4b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)
let times_3 (lst: int list): int list =
  map (fun x -> x * 3) lst
;;

(* 4c. Write a function that takes an int list and an int and multiplies every
 * entry in the list by the int. Use map. *)
let times_x (x: int) (lst: int list) : int list =
  map (fun y -> x * y) lst
;;

(* 4d. Rewrite times_3 in terms of times_x.
 * This should take very little code. *)
let times_3_shorter =
  times_x 3
;;

(* 4e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list. e.g. mult_table [1;2;3] => [[1; 2; 3]; [2; 4; 6]; [3; 6; 9]] *)
let mult_table (lst: int list) : int list list =
  fold_right (fun x y -> ((times_x x lst) :: y)) [] lst 
;;

let mult_table_ (lst: int list) : int list list =
  fold_right (fun x y -> (map (fun x' -> x' * x) lst) :: y) [] lst 
;;

(* 4f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use fold_right. *)
let and_list (lst: bool list) : bool =
  fold_right (fun x y -> x && y) true lst
;;

(* 4g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)
let or_list (lst: bool list) : bool =
  fold_right (fun x y -> x || y) false lst
;;

(* 4h. Write a function that takes a bool list list and returns its when
 * interpreted value as a boolean expression in conjunctive normal form
 * (CNF). A CNF expression is represented as a series of OR expressions
 * joined together by AND.
 * e.g. (x1 OR x2) AND (x3 OR x4 OR x5) AND (x6).
 * Use map and/or fold_right.
 * You may find it helpful to use and_list and or_list. *)

(* [[true; true]; [true;false]; [false;false] *)
(*  [[true]; [true]; [false]] *)
(*    true && true && false *)
(*      false *)
let cnf_list (lst: bool list list) : bool =
  fold_right (fun x y -> (fold_right (fun x' y' -> x' || y') false x) && y) true lst
;;

let cnf_list_ (lst : bool list list) : bool = 
  and_list (map or_list lst)
;;

assert(cnf_list [[true;false]; [true;true]; [false;false]] = false);;

(* 4i. Write a function that takes an expr list and returns true if and only
 * if every expr in the list represents a true Boolean expression. *)
let all_true (lst: expr list) : bool =
  fold_right (fun x y -> (eval x) && y) true lst   
;;

(* 4j. Write and_list to return a bool option,
 * where the empty list yields None. Use map and/or fold_right.
 * (How would we do this without map?)
 * You may find it helpful to use a function you wrote earlier. *)
let and_list_smarter (lst: bool list) : bool option =
  match lst with
  | [] -> None
  | _ :: _ -> Some (and_list lst)
;;

(* 4k. Write max_of_list from section 0:
 * Return the max of a list, or None if the list is empty. *)
let max_of_list (lst:int list) : int option =
  match lst with
  | [] -> None
  | hd :: tl -> Some(fold_right (fun x y -> (max x y)) hd tl)
;;

(* [8;1;2] *)
(* hd = 8 tl = [1;2] *)
(* f 1 (f 2 8)) *)
(* f 1 (8) *)
(* 8 *)

(* 4L. Write bounds from section 0:
 * Return the min and max of a list, or None if the list is empty. *)
let bounds (lst:int list) : (int * int) option =
  match lst with 
  | [] -> None
  | hd :: tl -> (Some (fold_right (fun x y -> let (l, r) = y in (min l x, max r x)) (hd, hd) lst))
;;

[1;5;2]

f 1 (f 5 (f 2 (1, 1)))

(* the return value here is different than in the release *)
let bounds_ (list : int list) : (int option * int option) = 
  fold_right (fun x (min, max) -> (min_option x min, max_option x max)) 
	     (None, None)
	     (map (fun x -> Some x) list)
;;

(* 4m. Fold fold fold
 * Higher order functions are a powerful tool, and fold_right in particular
 * is extremely powerful. In fact many higher order functions including
 * map, filter, and others can be written in terms of fold_right. Let's
 * implement several ourselves.
 *
 * Implement map and filter using fold_right.
 *)

let map f lst =
  fold_right (fun x y -> (f x) :: y) [] lst
;;

let filter f lst =
  fold_right (fun x y -> (if f x then (x :: y) else y)) [] lst 
;;

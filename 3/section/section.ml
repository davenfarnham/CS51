(* CS51 Section 3 *)

open Core.Std
(* ********************** Part 1 - Clean it up. ******************** *)

(* Over the course of your programming career, you will undoubtedly encounter
 * some poorly written code that is hard to read, hard to reason about, and
 * hard to modify.  With high probability, some of it will be your own code
 * that you're returning to six months after writing it :)  This exercise
 * is here to practice rewriting code to make it better, while at the same time
 * making sure that your new code has the same behavior as the old.
 *)

(* John Hacker recently learned OCaml, but has not taken CS51, and so
 * he doesn't know about map, fold_right or proper style. As a result, he
 * isn't aware that this horribly convoluted function he invented can
 * be written in one short, elegant line of code.
 *
 * Write a function that behaves identically but is simpler and written with
 * correct style.
 *
 * Hint: work by generating a series of simpler and simplerer "mystery"
 * functions. Don't try to jump to the "one short, elegant line of code"
 * in a single step. At each stage ensure (via testing) that
 * your new version of "mystery" works the same as the previous.
 *)

exception None_found of string;;
exception Len_List of string;;

let rec mystery (lists : 'a list list) =  if List.length lists = 0 then []
  else if List.length (List.hd_exn lists) = 0
  then mystery (List.tl_exn lists)
  else if List.length (List.hd_exn lists) = 1
  then let hd = List.hd_exn lists in
    ((List.hd_exn) hd) :: mystery (List.tl_exn lists)
  else let hd = List.hd_exn lists in
    (List.hd_exn) hd :: (mystery ((List.tl_exn hd)::(List.tl_exn lists)))
;;

let rec mystery_new (lists : 'a list list) =
  match lists with
  | [] -> []
  | hd :: tl -> hd @ mystery_new tl
;;


(* Here's one test. Is this really enough? *)
assert (let x = [[];[]] in mystery x = mystery_new x);;



(* ***************** Part 2 - Map and fold_right ************************ *)
(* Exercises from section 2 for those sections that didn't get
 * a chance to cover map and fold_right thoroughly. The subsections match
 * the subsection from section one (i.e. subsection 1c. corresponds to 4c.
 * from section 2 notes).
 *)

(* Map and fold_right *)
(* Exercise 1 *)
let rec fold_right f u xs =
  match xs with
  | [] -> u
  | hd::tl -> f hd (fold_right f u tl);;

let rec map f xs =
  match xs with
  | [] -> []
  | hd::tl -> f hd :: map f tl;;

(* 1a. Implement length in terms of fold_right.
 * length lst returns the length of lst. length [] = 0. *)
let length (lst: int list) : int =
  failwith "Not implemented"
;;

(* 1b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)
let times_3 (lst: int list): int list =
  failwith "Not implemented"
;;

(* 1c. Write a function that takes an int and an int list and multiplies every
 * entry in the list by the int. Use map. *)
let times_x (x : int) (lst: int list): int list =
  failwith "Not implemented"
;;

(* 1d. Rewrite times_3 in terms of times_x.
 * This should take very little code. *)
let times_3_shorter =
  failwith "Not implemented"
;;

(* 1e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list.  e.g. mult_table [1;2;3] => [[1; 2; 3]; [2; 4; 6]; [3; 6; 9]] *)
let mult_table (lst: int list) : int list list =
  failwith "Not implemented"
;;

(* 1f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use fold_right. *)
let and_list (lst: bool list) : bool =
  failwith "Not implemented"
;;

(* 1g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)
let or_list (lst: bool list) : bool =
  failwith "Not implemented"
;;

(* 1h.	 Write a function that takes a bool list list and returns
 * its value as a boolean expression in conjunctive normal form (CNF).
 * A CNF expression is represented as a series of OR expressions joined
 * together by AND.
 * e.g. (x1 OR x2) AND (x3 OR x4 OR x5) AND (x6).
 * Use map and/or fold_right.
 * You may find it helpful to use and_list and or_list. *)
let cnf_list (lst: bool list list) : bool =
  failwith "Not implemented"
;;

(* 1i. Write a function that takes an expr list and returns true if and only if
 * every expr in the list represents a true Boolean expression. *)

(* From section 2 *)

type expr = Value of bool | Not of expr | And of expr * expr | Or of expr * expr

let rec eval (a:expr) : bool =
  match a with
    | Value v -> v
    | Not e -> not (eval e)
    | And (e1, e2) -> eval e1 && eval e2
    | Or (e1, e2) -> eval e1 || eval e2
;;


let all_true (lst: expr list) : bool =
  failwith "Not implemented"
;;

(* You may find these helper functions from section 1 helpful. *)

let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =
  match (x, y) with
  | (Some x', Some y') -> Some (f x' y')
  | (_, None) -> x
  | (None, _) -> y
;;

let min_option x y = calc_option min x y ;;

let max_option x y = calc_option max x y ;;

let and_option x y = calc_option (&&) x y ;;

(* 1j. Write and_list to return a bool option,
 * where the empty list yields None. Use fold_right. *)
let and_list_smarter (lst: bool list) : bool option =
  failwith "Not implemented"
;;

let and_list_smarter' (lst: bool list) : bool option =
  failwith "Not implemented"
;;

(* 1k. Write max_of_list from section 1:
 * Return the max of a list, or None if the list is empty. *)
let max_of_list (lst:int list) : int option =
  match lst with
  | [] -> None
  | hd :: tl -> Some (List.fold_right ~f:(fun x y -> if y > x then y else x) ~init:(hd) tl)
;;

(* 1l. Write bounds from section 1:
 * Return the min and max of a list, or None if the list is empty. *)
(* we're going to write this without the help of previous functions, so we're going to get an unmatched error
   which we'll catch with an exception *)
let bounds (lst:int list) : (int option * int option) =
  match lst with
  | [] -> (None, None)
  | hd :: tl ->List.fold_right ~f:(fun x y -> 
				     (match y with
				      | (None, None) | (_, None) | (None, _) -> raise (None_found "Empty Tuple")
				      | (Some l, Some r) -> ((if x < l then Some x else Some l), (if x > r then Some x else Some r)))) 
				~init:(Some hd, Some hd) tl
;;

(* **************** Part 3 - More Map/Fold ******************* *)
(* For more practice writing and using higher order functions. *)

(* 3a. Fold fold fold
 * Higher order functions are a powerful tool, and fold_right in particular
 * is extremely powerful. In fact many higher order functions including
 * map, filter, and others can be written in terms of fold_right. Let's
 * implement several ourselves.
 *
 * Implement map, filter, and map2 using fold_right.
 *)
let map f lst =
  List.fold_right ~f:(fun x y -> f x :: y) ~init:[] lst
;;

let filter f lst =
  List.fold_right ~f:(fun x y -> if f x then x :: y else y) ~init:[] lst
;;

(* Challenge: map2 takes as input a two-argument function and two lists
 * and recurses through each list simultaneously, passing the hd of each list
 * as the arguments to the function, and spits out a new list with the return
 * values of the function.
 *
 * Hint: fold_right only works on one list at a time. What might you do to the
 * two lists so that fold_right can do its magic?
 *
 * Note: how should we handle lists of different lengths?
 *)
let rec map2_ f lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | ([], _) | (_, []) -> raise (Len_List "Unequal")
  | (hd :: tl, hd1 :: tl1) -> (f hd hd1) :: map2_ f tl tl1
;; 

let map2 f lst1 lst2 = 
  (* zip the two lists *)
  let rec zip (l1 : 'a list) (l2 : 'a list) : ('a * 'a) list = 
    match (l1, l2) with
    | ([], []) -> []
    | ([], _) | (_, []) -> raise (Len_List "Unequal")
    | (hd :: tl, hd1 :: tl1) -> (hd, hd1) :: (zip tl tl1) in
  List.fold_right ~f:(fun x y -> let (l, r) = x in (f l r) :: y) ~init:[] (zip lst1 lst2)
;;

(* 3b. filtermap
 * Write a function that takes
 *    -> a predicate, pred
 *    -> a one argument function f with argument type 'a
 *    -> a list of ('a)s, lst
 * The function should filter out items that make pred false, and
 * return the result of applying f on each element of the remaining
 * list.
 *
 * Your solution should use fold_right.
 *)
let filtermap (pred: 'a -> bool) (f: 'a -> 'b) (lst: 'a list) : 'b list =
  List.fold_right ~f:(fun x y -> if pred x then (f x) :: y else y) ~init:[] lst
;;

(* 2c.  Use filtermap to write the deoptionalize function from PS2.
   As a reminder:
   deoptionalize [None; Some 2; None; Some 3; Some 4; None] = [2;3;4] *)
let deoptionalize lst =
  filtermap (fun x -> not (x = None)) (fun x -> (match x with
						 | None -> raise (None_found "Found None")
						 | Some x' -> x')) lst
;;

(* You may have noticed that you needed to raise an exception to make
   deoptionalize work properly with arbitrary option types. There are
   very few situations where you shouldn't be doing a complete match,
   and where you should fall back on exceptions. Here is an alternative
   (much better) way to define filter_map that avoids this problem. Try
   filling in the code for this definition (use fold_right here too) *)

let filter_map (f: 'a -> 'b option) (lst: 'a list) : 'b list =
  List.fold_right ~f:(fun x y -> (match f x with
				  | None -> y
				  | Some x' -> (x' :: y))) ~init:[] lst
;;


(* Now write deoptionalize using this new filter_map *)
let deoptionalize' (lst) = filter_map (fun x -> x) lst  ;;

(* ******************* Part 42 - Substitution Model ******************* *)
(* The purpose of these exercises is to help you develop a more formal
 * model of how ocaml code evaluates.  This is useful both when writing
 * and reading programs.  For some relevant examples, see the entries
 * at the Underhanded C contest:
 * http://underhanded.xcott.com/?page_id=5
 *)

(* For each of these, replace ??? with code that will make the snippet
 * evaluate to the integer 42.  The code should be properly
 * parenthesized--things like ") in blah blah in blah (" are not allowed.
 *
 * If this is not possible, justify why not.  "I couldn't figure out how" isn't
 * a valid justification.  "No matter what you replace ??? with, this expression
 * cannot possibly evaluate to 42 because..."  is a good start.
 *
 * (Note: the style of indentation here is meant to emphasize the concept
 * of scope within the subsitution model, and does not necessarily match
 * standard styling)
 *)

(* from class *)
let a = 30 in
  (let a = 4 in 3 * a) + a;;  (* 42 *)

(* from class - change of parentheses*)
let a = 30 in
  (let a = 4 in 3 * a + a);; (* 16 - now the first 'a' is shadowed completely *)

(* 42.1 *)
let f = fun (x, _) -> x in
  f (42, 24) (* take a tuple as argument, return left element *)
;;

(* 42.11 *) (* or, if you insist on using the right element *)
let f = fun (_, y) -> y * 2 - 6 in
  f (42, 24) 
;;

(* 42.2 *)
let x = 14 in
  let x' = (fun x -> let x = x * 2 in x) in (* let x' = (fun x -> x * 2) *)
    List.fold_right ~f:(+) ~init:0 (List.filter ~f:(fun x -> x = x) [x' x; x]) (* List.filter basically does nothing *)

(* 42.3 *) (* partial application *)
let f = (fun x y -> x + y) in (* (fun x -> (fun y -> x + y)) *)
  let g = f 21 in             (* (fun x -> (fun y -> x + y)) 21 -> (fun y -> 21 + y) *)
    g 21
;;

(* 42.4 *) (* no *)
let f = (fun (x,y) -> x + y) in
  let g = f (???) in
    g 21
;;

(* 42.5.1 *)
let f = fun _ _ -> 42 in
  f f (f f)
;;

(* f f (f f) -> f f f -> two right functions are plugged into right one -> f _ _ -> 42 *)

(* (fun x y -> fun x -> fun y -> 42) (fun x y -> fun x -> fun y -> 42) *)
(* (fun x y -> fun x -> fun y -> 42) (fun x -> fun y -> 42) (fun y -> 42) *)
  
(* 42.5.2 *)
let f = fun _ _ _ -> 42  in
  (f f) f f
;;

(* the first f takes the f in parentheses as partial application, but then needs two more inputs *)

(* (fun x y -> fun x -> fun y -> 42) (fun x y -> fun x -> fun y -> 42) *)
(* fun y -> 42 (fun x y -> fun x -> fun y -> 42) (fun x y -> fun x -> fun y -> 42) *)
(* this won't work because you're trying to apply two arguements that only has one parameter *)

(* 42.5.3 *)
let f = fun _ _ _  -> 42 in
  f f f f
;;

(* f f f f -> f _ _ _ -> 42 *)

(* From now on, your definitions cannot contain the value "42"! *)

(* 42.6 *)
let f =  (fun _ y -> y * 2) in
  List.fold_right ~f:f ~init:21 [f] (* yes, you can have an array of functions *)
;;

(* (f f 21) *)

(* 42.x: Bonus *)
let thequestion = 7 in
  6 * thequestion
;;

(* Extra questions (from quizzes) *)

(* What does this evalute to? *)
let x = 3 in
   x + 5
;; (* 8 *)

let x = 3 in
 let x = 4 in
   x + 5
;; (* 9 *)

let x = 3 in
 let x = x + 2 in
   x + 5
;;
(* x = 3 + 2 -> 5 *)
(*   5 + 5 -> 10; different than a function, really, just binding values *)

let f x = x + 1 in (* fun x -> x + 1 *)
 let f x = if x = 0 then 1 else x * f (x - 1) in
   f 5
;;
(* let f x = if x = 0 then 1 else x * (fun x -> x + 1) (x - 1) *) 
(* 25 *)

let x = 3 in
  let f y = y + x in (* (fun y -> y + 3) *)
   let x = 5 in
     f 1 (* (fun y -> y + 3) 1 *)
;;
(* 4 *)

List.fold_right [1; 2; 3] ~f:(-) ~init:0;;
(* Solution *)
(* Answer: 2 *)

(* Why: *)
(* (f 1 (f 2 (f 3 0))) *)
  (* (fun x y -> x - y) (3) (0) -> 3 - 0 -> 3 *)
    (* (fun x y -> x - y) (2) (3) -> 2 - 3 -> -1 *)
      (* (fun x y -> x - y) (1) (-1) 1 - (-1) -> 2 *)

(* make these come out to 51 *)

(* 51 *)
let inc = (fun x -> x + 1) in
  let mul2 = (fun x -> x * 2) in
	let lst = [7;8;9] in (* 20 + 18 + 14 - 1 *)

	(* way one *)
	(*
          List.fold_right ~f:(fun x y -> if x = 8 || x = 9 then mul2 (inc x) + y
			                 else mul2 x - 1 + y) ~init:0 lst 
	*)
	(* way two *)
          List.fold_right ~f:(fun x y -> inc x + y) ~init:0 (List.map ~f:(fun x -> mul2 x) lst) 
;;

(* 51.1 *)
let f = (fun x -> x + 1) in
  let f = (fun x -> (f x) + 1) in (* (fun x -> (fun x -> x + 1) x + 1 *)
    f 49
;;

(* what's the correct reduction *)
let inc n = n + 1 in
  inc (inc 3)

(* Solution *)
(* inc = (fun n -> n + 1 *)
(* ((fun n -> n + 1) ((fun n -> n + 1) 3)) *)
(* ((fun n -> n + 1) (3 + 1)) *)
(* ((fun n -> n + 1) 4) *)
(* 4 + 1 *)
(* 5 *)   

(* using this record, return "RobDaven" *)
type person = {age : int; name : string; major : string};;

let awesomepeople = 
  [{age = 23; name = "Rob"; major="CS"};
   {age=27;name="Daven";major="Chinese"};
   {age=22;name="Ben";major="Physics"}] in
     List.fold_right ~f:(fun x y -> if x.name = "Daven" || x.name = "Rob" then x.name ^ y else y) ~init:"" awesomepeople
;;

(* ******************* Part 5 - RSA Encryption ******************* *)
(* RSA Encryption - You've used it with git, you might use it with ssh,
 * you will implement a version of it on ps3, but what is it exactly?
 *
 * Let's start with a more general question: What is a cryptographic system?
 * A cryptographic system is a system designed to keep certain data/information
 * private by encoding that data/information and only allowing certain
 * individuals/groups to decode that data/information. A simple example
 * seen by many of you in CS51 is Caesar's cipher implemented below over
 * the ASCII character set:
 *)

(*
let caesar_encrypt (key: int) (ptext: string) : string =
  String.map ~f:(fun c -> Char.of_int_exn ((Char.to_int c + key) mod 256)) ptext
;;

let caesar_decrypt (key: int)  (ctext: string) : string =
  caesar_encrypt (256 - (key mod 256)) ctext;;
;;

*)

(* See how much simpler this in OCaml than C! Higher order functions FTW! *)

(* Caesar's cypher requires the same key for both encryption and decryption. Can
 * you see any scenario when this might be a problem? What if we used
 * different keys for encryption and decryption? This is the concept behind
 * public key encryption, an idea pioneered by Diffee and Hellman in 1976. In
 * public key encryption, the encryption key is made public and the decryption
 * key is kept private. Furthermore, knowing the encryption key cannot help
 * you find the decryption key. Therefore, someone can encode a message with
 * your public encryption key, send it to you, and know that only you can read
 * it!
 *
 * Is it possible to implement Caesar's cipher as the algorithm behind
 * a public key encryption system? Why or why not?
 *
 * RSA encryption is one of the most popular types of public key encryption.
 * Aside from being a public key encryption system, however, what is it
 * that makes RSA encryption effective? RSA encryption takes advantage
 * of the fact that there is no known algorithm for factoring large numbers
 * efficiently. How you might ask? Check out the pset! *)

(* Note: on the pset we have provided a lot of boiler plate code to get you
 * started. At first this may be overwhelming, but we didn't name our
 * functions "mystery" for a reason. Our comments might also prove quite
 * useful! *)

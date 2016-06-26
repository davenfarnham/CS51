open Core.Std

(* Here we provide a definition for infinite binary treestreams. This
 * definition is analogous to the definition of streams provided in
 * lecture. *)

type 'a tree = unit -> 'a tr
and 'a tr = Stem of 'a * 'a tree * 'a tree ;;

(*>* Problem 2.1.a *>*)
(* Write a function headt which takes an 'a treestream and returns the value
 * at the root of the tree *)

let headt (t: 'a tree) : 'a =
  match t () with
  | Stem (a, _, _) -> a
;;

(*>* Problem 2.1.b *>*)
(* Write functions ltail and rtail which take a treestream and return its
 * left and right subtrees respectively *)

let ltail (t: 'a tree) : 'a tree =
  match t () with
  | Stem (_, l, _) -> l
;;

let rtail (t: 'a tree) : 'a tree =
  match t () with
  | Stem (_, _, r) -> r
;;

(*>* Problem 2.1.c *>*)
(* Write a function mapt which takes takes a function f and maps it
 * over the given treestream *)

let rec mapt (f: 'a -> 'b) (t: 'a tree) : 'b tree =
  match t () with
  | Stem (a, l, r) -> fun () -> Stem (f a, mapt f l, mapt f r)
;;

(*>* Problem 2.1.d *>*)
(* Write a function zipt which takes a function f and two treestreams
 * t1 and t2 and combines them into one treestream. If x1 and x2 are the
 * values at corresponding locations in t1 and t2 respectively, then
 * the corresponding value in "zipt f t1 t2" should be "f x1 x2" *)

let rec zipt (f: 'a -> 'b -> 'c) (t1: 'a tree) (t2: 'b tree) : 'c tree =
  match (t1 (), t2 ()) with
  | Stem (a, l, r), Stem(a', l', r') -> fun () -> Stem(f a a', zipt f l l', zipt f r r')
;;

(* Define a treestream of all ones *)

(*>* Problem 2.1.e *>*)
let rec onest () =
  Stem(1, onest, onest)
;;

let _ = 
  let twos = zipt (fun x y -> x + y) onest onest in
  assert(headt twos = 2);
  assert(headt (ltail twos) = 2); 
  assert(headt (rtail twos) = 2)
 

(* Define a treestream in which each positive natural number appears
 * exactly once, and where the first few rows are 1; 2 3; 4 5 6 7,
 * etc. This should look something like the following

           1
         /   \
       2       3
     /   \   /   \
    4    5   6    7
   / \  / \ / \  / \
          ...

 *)

(*>* Problem 2.1.f *>*)
let rec treenats () =
  let rec loop t () =
    let h = headt t in
      Stem(h, loop (mapt (fun x -> x * 2) (ltail t)), loop (mapt (fun x -> x * 2 + 1) (rtail t))) in
  loop onest
;;

let _ = 
  assert(headt (treenats ()) = 1);
  assert(headt (ltail (ltail (treenats ()))) = 4); 
  assert(headt (rtail (ltail (treenats ()))) = 5);
  assert(headt (ltail (rtail (treenats ()))) = 6);
  assert(headt (rtail (rtail (treenats ()))) = 7); 
  assert(headt (ltail (ltail (ltail (treenats ())))) = 8)


(***************** Using the Lazy module ******************)
(* Here we provide an alternate implementation of streams using
 * OCaml's lazy module. We recommend that you explore the
 * documentation at
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html

 * In this portion, you will be reimplementing various functions that
 * were defined in class.
 *)

open Lazy

type 'a stream = Cons of 'a * 'a stream Lazy.t

let rec ones = Cons(1, lazy(ones));;

(*>* Problem 2.2.a *>*)
(* Implement the head function *)

let head (s:'a stream) : 'a =
  match s with
  | Cons(a, _) -> a
;;

(*>* Problem 2.2.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  match s with
  | Cons(a, s') -> Cons(f a, lazy(map f (Lazy.force s')))
;;

(*>* Problem 2.2.c *>*)
(* Define nats *)

let rec nats = Cons(1, lazy(map (fun x -> x + 1) nats)) ;;

(*>* Problem 2.2.d *>*)
(* Write a function nth, which returns the nth element of a
 * stream. NOTE: the function nth should be zero-indexed. In other
 * words, "nth 0 s" should be equivalent to "head s". *)
let rec nth (n:int) (s:'a stream) : 'a =
  if n = 0 then head s else (match s with
			     | Cons(a, s') -> nth (n - 1) (Lazy.force s'))
;;

let _ = 
  assert(nth 0 nats = 1);
  assert(nth 1 nats = 2);
  assert(nth 2 nats = 3);
  assert(nth 3 nats = 4)


(*>* Problem 2.2.e *>*)
(* Now suppose we have two int streams s1 and s2 sorted in ascending
 * order. We wish to merge these into a single stream s such that s is
 * sorted in ascending order and has no duplicates. Implement this
 * function. NOTE: This is not a simple merge function. You must
 * REMOVE DUPLICATES *)

let merge (s1:int stream) (s2:int stream) : int stream =
  let rec loop s1' s2' = 
    match s1', s2' with
    | Cons(a, s1''), Cons(b, s2'') -> (match a < b with
				       | true -> Cons(a, lazy(loop (Lazy.force s1'') s2'))
				       | false -> if a = b then Cons(a, lazy(loop (Lazy.force s1'') (Lazy.force s2'')))
					          else Cons(b, lazy(loop s1' (Lazy.force s2'')))) in
  loop s1 s2
;;

let rec evens = Cons(2, lazy(map (fun x -> x + 2) evens))
let rec odds = Cons(1, lazy(map (fun x -> x + 2) odds))

let _ = 
  assert(nth 0 (merge evens odds) = 1);
  assert(nth 1 (merge evens odds) = 2);
  assert(nth 2 (merge evens odds) = 3);
  assert(nth 3 (merge evens odds) = 4);
  assert(nth 0 (merge ones ones) = 1);
  assert(nth 1 (merge ones ones) = 1);
  assert(nth 2 (merge ones ones) = 1)


(*>* Problem 2.2.f *>*)
(* What problems can we run into with this conception of "merge"? What
 * if we were to run "merge ones ones"? Answer within the comment. *)

(*
 *  Answer: In my implementation, it doesn't actually merge, it just returns stream a, effectively
 *)

(*>* Problem 2.2.g *>*)
(* Write a function "scale", which takes an integer "n" and an int
 * stream "s", and multiplies each element of "s" by "n". *)

let scale n = map (fun x -> x * n) ;;

(*>* Problem 2.2.h *>*)
(* Suppose we wish to create a stream of the positive integers "n" in
 * increasing order such that any prime factor of "n" is either 3 or
 * 5. The first few numbers are 1, 3, 5, 9, 15, 25, 27, ... hopefully
 * you get the idea. One way to do this is to run "filter" over
 * "nats". But we can do better than that. *)

(* Let "selectivestream" denote the desired stream. Observe that
   "selectivestream" satisfies the following properties

   1. The elements of "scale S 3" are elements of "selectivestream"
   2. The elements of "scale S 5" are elements of "selectivestream"
   3. "selectivestream" is the minimal stream (sorted in increasing order) which
   contains "1" and satisfies the above properties

   Think about why properties 1-3 uniquely characterize "selectivestream".
*)

(* Use the above discussion and functions you've already written to
   give a simple definition for "selectivestream". This can be done quite
   elegantly. *)

let rec selectivestream = Cons(1, lazy (merge (scale 3 selectivestream) (scale 5 selectivestream))) ;;

let _ = 
  assert(nth 0 selectivestream = 1);
  assert(nth 1 selectivestream = 3);
  assert(nth 2 selectivestream = 5);
  assert(nth 3 selectivestream = 9);
  assert(nth 4 selectivestream = 15);
  assert(nth 5 selectivestream = 25);
  assert(nth 6 selectivestream = 27)


(*>* Problem 2.3 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 51

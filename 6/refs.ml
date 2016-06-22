(* NAMES:
 *
 * Partner 1's name: ______
 * Partner 1's code.seas account: _______
 *
 * (Leave blank if you are working alone)
 * Partner 2's name: ______
 * Partner 2's code.seas account: _______
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  failwith "Unimplemented"

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  failwith "Unimplemented"

(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  failwith "Unimplemented"

(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = -1

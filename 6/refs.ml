open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(* helper function checking for loops (duplicate nodes in list) *)
let rec check_lst v i =
  match v with
  | [] -> false
  | hd :: tl -> if (phys_equal hd !i) then true else check_lst tl i
;;

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  let rec loop l' visited = 
    match l' with
    | Nil -> false
    | Cons(_, c) -> if (check_lst (l' :: visited) c) then true else (loop !c (l' :: visited)) in
  loop lst []
;;

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

let _ = 
  assert(has_cycle list1a = false);
  assert(has_cycle list1b = false);
  assert(has_cycle list1 = false);
  assert(has_cycle !reflist = true);
  assert(has_cycle list2 = true)


(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec loop l' visited =
    match l' with
    | Nil -> ()
    | Cons(_, c) -> if (check_lst (l' :: visited) c) then (c := Nil) else (loop !c (l' :: visited)) in
  loop lst []
;;

let _ = 
  assert((flatten list2); has_cycle list2 = false);
  assert(list2 = Cons(1, ref (Cons (2, ref Nil))))
	
let list4a = ref (Cons (4, ref Nil)) 
let list5a = ref (Cons (3, list4a)) 
let list3a = Cons(1, ref (Cons (2, list5a))) 
let _ = list4a := !list5a

let _ = 
  assert (has_cycle list3a = true);
  assert ((flatten list3a); has_cycle list3a = false);
  assert (list3a = Cons(1, ref (Cons (2, ref (Cons (3, ref Nil))))))


(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  let rec loop l' visited count =
    match l' with
    | Nil -> count
    | Cons(_, c) -> if (check_lst (l' :: visited) c) then (count + 1) else (loop !c (l' :: visited) (count + 1)) in
  loop lst [] 0
;;

let _ = 
  assert (mlength list3a = 3);
  assert (mlength list2 = 2)

let list4b = ref (Cons (4, ref Nil)) 
let list5b = ref (Cons (3, list4b)) 
let list3b = Cons(1, ref (Cons (2, list5b)))

let _ = 
  assert (mlength list3b = 4);
  list4b := !list5b;
  assert (mlength list3b = 3)


(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 51

(*
 * CS 51 Section 9
 * Intro to Concurrency
 *)

open Core.Std

(* Define the following terms:

    - Concurrency




    - Parallelism




    - Thread




    - Critical section




    - Lock / Mutex




    - Deadlock




    - Atomic




*)





(* Exercise 0.1
 *
 * Threads make life complicated!  Why do we bother?
 *)





(* Exercise 0.2
 *
 * What are futures? How do they differ from threads? When might you use futures?
 *)




(* In OCaml, a concurrent program uses threads to interleave execution
 * of different parts of the program. Threads are created using Thread.create
 * of the Thread module. In the programs you've written so far, all your code
 * has run in a main thread that was created by default. If a Thread is
 * created, it begins executing the code in the function you pass to
 * Thread.create.  Calling Thread.join with the thread id that Thread.create
 * returns causes the main thread to wait until the other thread finishes.
 *
 * We may reason about order of execution more easily by defining
 * "happens-before" relationships. Given a Thread t,
 *   - A call to Thread.create happens before any statement executed by
 *     the thread
 *   - If Thread.join is called on thread t, all statements executed by
 *     t happen before Thread.join returns
 *
 *)


(* Exercise 1. *)

(* What's wrong with the following code? *)


let f () =
    let x = ref 0 in
    let inc x = x := !x + 1 in

    let rec g repeat () =
        if repeat <= 0 then () else
        let _ = inc x in
        g (repeat-1) () in

    let thread = Thread.create (fun _ -> g 1000000 ()) () in
    let _ = g 1000000 () in
    Thread.join thread;
    print_int !x; print_string "\n"

let _ = f ()


(* What's one way of fixing this program? Insert the fix into the program.
 * What does this do to the parallelism of our code?
 *)


(* Try to reason about all of the possible interleavings of the functions
 * x and y below, and the possible final values of num (assuming that only
 * valid interleavings can produce final values, which may not be the case,
 * and in fact probably isn't the case on your own computer).
 *)


let f () =
    let num = ref 0 in
    let inc () = num := !num + 1 in

    let x () =
        let _ =
            if (!num = 0) then inc ()
            else if (!num = 2) then (inc (); inc ())
            else inc(); inc(); inc() in
        () in

    let y () =
        inc ();
        let _ = if (!num = 1) then inc () in
        () in

    let thread = Thread.create y () in
    let _ = x () in
    Thread.join thread;





(* Exercise 2: Linked Lists *)

(*
 * Remember from lecture that divide and conquer algorithms are not very
 * effective when dealing with lists, since there is no constant-time way
 * of dividing the list into parts. However, we may still want to be able
 * to access and modify a list safely from multiple threads. Here we'll define
 * such a list data structure.
 *)

class type ['a] linked_list =
object
    (* Adds an element to the front of the list *)
    method push : 'a -> unit

    (* Removes and returns the element from the front of the list *)
    method pop : 'a option

    (* Returns true if the given element is in the list, false otherwise *)
    method search : 'a -> bool

    (* Removes the first occurrence of the given element in the list.
     * Returns the original list if such an element is not found *)
    method remove : 'a -> unit

end

(* If we wanted, we could keep a lock on the entire list, in which case all
 * list operations would readily be atomic. Why might we not want to do that?
 *)

(* Instead, associate a mutex with each element in the list *)
type 'a node = Nil | Cons of 'a * 'a list
and 'a list = {lock : Mutex.t; mutable node : 'a node}

(* The following functions from lecture might be useful. *)

let with_lock (l:Mutex.t) (f:unit -> 'a) : 'a =
    let _ = Mutex.lock l in
    let res = try f () with exn -> (Mutex.unlock l ; raise exn) in
    let _ = Mutex.unlock l in
    res


let new_id : unit -> int =
    let c = ref 0 in
    let l = Mutex.create() in
    (fun _ -> with_lock l (fun () -> (c := (!c) + 1; !c)))

(*
 * Do we actually need new_id? What did we originally use it for? Why might
 * that not matter in this case?
 *)

class ['a] llist : ['a] linked_list =
object (this)


    (* Why do we still need a super lock? *)
    val super_lock : Mutex.t = Mutex.create()
    val mutable contents : 'a list = {lock = Mutex.create(); node =  Nil}

    method push x =







    method pop =







    method search x =









    method remove x =











end

(* Summary:
 *
 * Reasoning about concurrent programming is hard.  You should now
 * have a sense of why, and should be able to see concurrency problems
 * in toy examples.  If you want to learn more, take CS61.  If you
 * want to really understand concurrency, take cs161.
 *)

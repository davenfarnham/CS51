(* CS51 Spring 2015
Section 6 Notes
Week of 03/29/15 *)

open Core.Std

(******************  Part 1: Mutation and State  ******************)
(* So far in this course, we have worked almost exclusively with the
 * subset of ML that is functional (with the exceptions of exceptions
 * (sorry) and printing). We have traded iteration counters for
 * recursion, among other things.
 *
 * Recall from lecture that, while there are plenty of
 * reasons to sing the praises of functional programming, eventually
 * we need side-effects. After all, we write programs in order to
 * change the world, and any change to the world is necessarily a side-effect
 * of functional computation.
 *
 * The basic way of allowing for mutable state in ML is to use references.
 * To recap from lecture:
 *
 * New type: t ref
 * -> Think of it as a pointer to a box that holds a t value.
 * -> The pointer can be shared.
 * -> The contents of the box can be read or written.
 *
 * To create a fresh box: ref 42
 * -> allocates a new box, initializes its contents to 42, and returns a
 *    pointer to that box.
 *
 * To read the contents: !r
 * -> If r points to a box containing 42, then !r returns 42.
 * -> similar to *r in C
 *
 * To write the contents: r := 42
 * -> updates the box that r points to so that it contains 42.
 * -> similar to *r = 42 in C
 *
 * Note that:
 * Now that we are updating state, sometimes we will want to have a sequence
 * of expressions, but we will only care about the value of the last
 * expression. We accomplish this with semicolons.
 * For example, (a := 5 + b; !a) would update the "box" that a points to
 * to have a value of 5 more than the value of b, and then evaluate
 * to this same value. You should only use semicolons when the value on the
 * left is of type unit.
 *
 *)

(* 1.1: Vocabulary Check
 *
 * What is the name for something that becomes a problem in the presense of
 * references and mutation, defined by Wikipedia as "situation in which
 * a data location in memory can be accessed through different symbolic
 * names in the program"?
 *)

(* 1.2: 42 Exercises *)
(* Replace ??? so that the expression evaluates to 42. *)
(* 1.2.1 *)

(* turn the function into a reference *)
let f = (fun () -> [ref (fun x -> x * 2)]) in
  match f () with
    | [] -> 12
    | a::b -> !a 21
;;

(* 1.2.2 *)
(* use a ref to a boolean to switch things around *)
let f = let x = ref false in (fun () -> (if !x then 42 else (x := true; 0))) in
  if f () = 42 then 21 else f ()
;;

(* recursive loop *)
let rec fun_loop x : unit =
  if x < 0 then printf "done!\n" 
  else((printf "I'm currently on my %i loop!\n" x); fun_loop (x-1))
;; 

(* loop using a mutable state, like in C *)
let ref_loop x : unit =
  let count = ref x in
  (* notice I'm not passing a counter into loop as an arguement - hence, everytime I change count, it stays changed *)
  let rec loop () : unit = if !count < 0 then printf "done!\n" 
                           else((printf "I'm currently on my %i loop!\n" !count); ((count := !count - 1); loop ()))
  in loop ()
;; 

(* 1.3: Call Counter
 * Write call_counter : ('a -> 'b) -> (('a->'b)*int ref)
 * The second component of the returned pair should
 * contain the number of times the first component has
 * been called. *)
let call_counter (f : 'a -> 'b) : (('a -> 'b) * int ref) =
  let counter = ref 0 in
    ((fun x -> (counter := !counter + 1; f x)), counter) (* update the counter on the left multiple times *)
;;

(* what does the left hand side do? set counter = counter + 1, then on the right call the the function f on x *)
(* your'e passing in the function x, which increments the ref, to f, so the below *)

let (square, square_cnt) = call_counter (fun x -> x * x);;
let (cube, cube_cnt) = call_counter (fun x -> x * x * x);;

cube 3;;
!cube_cnt;;

square 2;;

cube (square 2);;
square 2 -> (4, 1)

!square_cnt;;
!cube_cnt;;

(* 1.4: Aliasing
 * What does x evaluate to?
 *)

let x =
  let r = ref 1 in
  let rs = [r;r;r] in
  List.map ~f:(fun x -> x := (!x) + 1; !x) rs

(* [2;3;4] *)

(************************* Part 2: Streams ***************************)
(* Recall our definition of the stream datatype, and some
 * basic operations we defined on streams.
 *)

(* so here we have a mutually recursive definition. 'a str = is simply the concatenation of one value, 'a, onto a 'a stream. Well, what's a 'a stream?"
   A 'a stream is a function that takes a unit, and returns a 'a str, which is the concatenation of one value, 'a, onto a 'a stream. Well what's a 'a stream?...... 
   And so on for infinity and beyond! *)
type 'a str = Cons of 'a * ('a stream) 
 and 'a stream = unit -> 'a str

(* well, what would this look like concretely? here's an infinite list of ones. *)
let rec ones : int str = Cons (1, fun () -> ones);;

let head (s : 'a stream) : 'a =
  let Cons (h, _) = s () in h
;;

let tail (s : 'a stream) : 'a stream =
  let Cons(_, t) = s () in t
;;

let rec bad_map (f : 'a->'b) (s : 'a stream) : 'b stream =
  let Cons (h, t) = s () in (* this line will call s (), which will loop infinitely *)
                            (*  it works with the below since you're wrapping everything in fun () -> *)
    fun () -> Cons (f h, map f t)
;;

let rec map (f : 'a -> 'b) (s : 'a stream) : 'b stream = 
  fun () -> Cons (f (head s), map f (tail s)) 
;;

(* using some of the above functions *)
let rec filter p s =
  let h = head s in
    if p h then Cons (h, (fun () -> (filter p (tail s)))) else (filter p (tail s))
;;

(* 2.1 *)
(* What happens if p doesn't match anything in the stream? *)


(* Here are some useful streams: *)
let rec ones =
  fun () -> Cons(1, ones)
;;

let rec nats =
  fun () -> Cons(0, map (fun x -> x + 1) nats)
;;

(* 2.2 *)
(* Define a stream that contains all integer multiples of 3. *)
let mult3 =
  fun () -> filter (fun x -> x % 3 = 0 && x > 0) nats
 ;;

(* 2.3 *)
(* Write a function that takes an integer n and a stream
 * and returns a list containing the first n elements of the stream. *)
let rec first (n : int) (s : 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ ->  let Cons(h, t) = s () in (h :: (first (n-1) t))
;;

(* 2.4.1 *)
(* Define a function alternate that takes the negative of every other
 * item in a stream, starting with the second. e.g., 1,1,1,1... would
 * become 1,-1,1,-1,... *)
let rec alternate (s : float stream) : float stream =
  let Cons(h, t) = s() in fun () -> Cons (h, map (fun x -> x *. -1.) (alternate t))
;;

(* 2.4.2 *)
(* Another way to write alternate, without using map? *)
let rec alternate' (s : float stream) : float stream =
  let c = ref 0 in
  let rec loop (s' : float stream) : float stream = 
    let Cons(h, t) = s' () in fun () -> let h' = (if !c mod 2 = 0 then h else h *. -1.) in (Cons (h', (c := !c + 1; (loop t)))) in
  loop s
;;

(* 2.5 *)
(* Define a stream that contains the alternating harmonic sequence, whose
 * nth term is (-1)^(n+1)(1/n), e.g., 1, -1/2, 1/3, -1/4...
 * You may use nats. *)
let altharm : float stream =
  let nats_f = tail (map (fun x -> Float.of_int x) nats) in (* turn nats into a float stream *)
    let Cons(h, t) = nats_f () in fun () -> Cons(h, map (fun x -> (-1. ** (x +. 1.) *. (1. /. x))) t)
;;

(* 2.6 *)
(* Write a function streammax that takes two streams and returns
a new stream of the maximum of the first elements of s1 and s2, followed
by the maximum of the second element of each, and so on. (For example,
s1 and s2 might represent simultaneous rolls of two dice, and we want a
stream representing the maximum in each pair of rolls.) *)
let rec streammax (s1 : int stream) (s2 : int stream) : int stream =
  let Cons(h1, t1) = s1 () in
  let Cons(h2, t2) = s2 () in 
  fun () -> Cons((if h1 > h2 then h1 else h2), streammax t1 t2)
;;

(* 2.7 - Collatz Conjecture *)
(* Consider the following procedure:
 * Take an integer. If the integer is odd, multiply by 3 and add 1.
 * If it is even, divide it by 2. Repeat this procedure.
 * The Collatz Conjecture states that if this procedure is repeated, starting
 * with any positive integer, it will eventually reach 1.
 *
 * For more information, see this scholarly article:
 * http://en.wikipedia.org/wiki/Collatz_conjecture, or this scholarly
n * webcomic: http://www.xkcd.com/710/. *)

(* 2.7.1 *)
(* Write a function, collatz, that takes an integer and returns an int
 * stream with the integers that result from performing the above process.
 * Since streams must be infinite according to our definition, once (or if)
 * 1 is reached, the rest of the stream should be ones. *)

let even x = (x mod 2 = 0);;
let col x = if x = 1 then 1 
	    else (if even x then x / 2 else 3 * x + 1);;

let rec collatz (n : int) : int stream =
  fun () -> Cons(n, collatz (col n))
;;

let rec collatz' (n: int) : int stream =
  fun () -> 
    let next_n =
      if n = 1 then 1 
      else if even n then n / 2 else 3 * n + 1 in
      Cons(n, collatz next_n)
;;

(* 2.7.2 *)
(* We can define a stream of streams with the collatz streams starting
 * with each natural number. *)

let collatzes = map collatz nats;;

(* And a predicate that determines if a stream contains 1. *)
let rec hasone s =
  (head s = 1) || (hasone (tail s))
;;

(* Now consider the following definition:
 *
 * let collatzproof = filter (fun s -> not (hasone s)) collatzes;;
 *
 * collatzproof is then a stream of streams which contains any collatz stream
 * that does not contain the number 1. If the stream collatzproof has an
 * element, the Collatz conjecture is false. If it is empty, the conjecture
 * is true. *)

(* Why can't we use this to prove or disprove the Collatz conjecture? *)




(************************* Part 3: Memoization ***************************)
(* Every time we take elements of a stream, they must be recomputed. This
 * results in extra processing time. A solution to this is memoization. *)

(* Exercise 3.1 *)
(* Consider the following stream. *)
let rec randstream =
  (fun () -> let random = Random.int(100) in Cons(random, randstream));;
let a = head randstream;;
let b = head randstream;;
(* Are a and b necessarily equal? Why or why not? What if we use
 * memoized streams? *)


(* Exercise 3.2 *)
(* What's the tradeoff? When might we might not want to memoize? *)

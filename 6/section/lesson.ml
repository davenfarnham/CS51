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

let test (n: int) : (unit * int ref) = 
  let count = ref 0 in
    if n = !count then ((printf "%i\n" !count), count) else (count := !count + 1; (printf "%i\n" !count); (test (n-1))) in
  test 5; test 4;;

let rec rec_loop (n: int) : unit = 
  if n = 0 then printf ""
  else (printf "hello\n"; rec_loop (n-1))
;;

let ref_loop (n: int) : unit =
let count = ref n in 
  let rec loop () = 
    if !count = 0 then printf ""
    else (printf "hello\n"; count := !count - 1; loop ()) in
  loop ()
;;

(* 1.2: 42 Exercises *)
(* Replace ??? so that the expression evaluates to 42. *)
(* 1.2.1 *)
let f = (fun () -> [ref (fun x -> x * 2)]) in
  match f () with
    | [] -> 12
    | a::b -> !a 21 
;;

(* 1.2.2 *)
(* I'm not calling this function again, as in a recursive call - instead I'm passing the unit value into g multiple times which gets evaluated in the anonymous function part.  *)
let g = (let x = ref false in fun () -> if !x then 42 else (x := true; 1)) in (* so g equals this entire statement *)
  if g () = 42 then 21 else g ()
;;

(* 1.3: Call Counter
 * Write call_counter : ('a -> 'b) -> (('a->'b)*int ref)
 * The second component of the returned pair should
 * contain the number of times the first component has
 * been called. *)
let call_counter (f : 'a -> 'b) : (('a -> 'b) * int ref) =
  let counter = ref 0 in
    ((fun x -> (counter := !counter + 1; f x)), counter)
;;

let (square, s_cnt) = call_counter (fun x -> x * x);;
let (cube, c_cnt) = call_counter (fun x -> x * x * x);;

square 2;;
!s_cnt;;

cube 2;;
!c_cnt;;

cube (square 2);;
!s_cnt;;
!c_cnt;;

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
type 'a str = Cons of 'a * 'a stream 
and 'a stream = unit -> 'a str (* the unit ensures the laziness *)

let rec ones : 'a stream = fun () -> Cons(1, ones);;

let head (s : 'a stream) : 'a =
  match s () with
  | Cons(h, _) -> h
;;

let tail (s : 'a stream) : 'a stream =
  match s () with
  | Cons(_, t) -> t
;;

(* bad implementation *)
let rec map (f : 'a->'b) (s : 'a stream) : 'b stream =
  let Cons(h, t) = s () in
  fun () -> Cons(f h, map f t)
;;

let rec map (f : 'a->'b) (s : 'a stream) : 'b stream =
  fun () -> Cons(f (head s), map f (tail s))
;;

let rec filter p s =
  let h = head s in
    let t = tail s in
  (if p h then (fun () -> Cons(h, filter p t)) else (filter p t))
;;

(* 2.1 *)
(* What happens if p doesn't match anything in the stream? *)

(* 9/4 mc^2 (hw) *)

(* Here are some useful streams: *)
let rec ones = fun () -> Cons(1, ones)
;;

let rec nats = fun () -> Cons(1, map (fun x -> x + 1) nats)
;;

(* 2.2 *)
(* Define a stream that contains all integer multiples of 3. *)

let mult3 = filter (fun x -> x % 3 = 0) nats
;;

(* 2.3 *)
(* Write a function that takes an integer n and a stream
 * and returns a list containing the first n elements of the stream. *)
let rec first (n : int) (s : 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (head s) :: first (n-1) (tail s) 
;;

(* 2.4.1 *)
(* Define a function alternate that takes the negative of every other
 * item in a stream, starting with the second. e.g., 1,1,1,1... would
 * become 1,-1,1,-1,... *)
let ones_f = map (fun x -> Float.of_int x) ones;;

let rec alternate (s : float stream) : float stream =  
  fun () -> Cons(1., map (fun x -> x *. -1.) (alternate s))
;;

(* 2.4.2 *)
(* Another way to write alternate, without using map? *)
let rec alternate' (s : float stream) : float stream =
  let Cons(a, b) = s () in
  let Cons(c, d) = b () in
    fun () -> Cons(a, (fun () -> Cons(-.c, alternate' d)))
;;

(* 2.5 *)
(* Define a stream that contains the alternating harmonic sequence, whose
 * nth term is (-1)^(n+1)(1/n), e.g., 1, -1/2, 1/3, -1/4...
 * You may use nats. *)
let seq n = (-1.) ** (n +. 1.) *. (1./.n);;

let nats_f = map(fun x -> Float.of_int x) nats;;

let altharm : float stream =
  map (fun x -> seq x) nats_f
;;

type 'a iterator = {hasNext : unit -> bool; next : unit -> 'a}

let stream_iter (s : 'a stream) : 'a iterator = 
  let sr = ref s in
  { hasNext = (fun () -> true);
    next = (fun () -> let Cons(h, t) = (!sr) () in sr := t; h) }
;;

(* 2.6 *)
(* Write a function streammax that takes two streams and returns
a new stream of the maximum of the first elements of s1 and s2, followed
by the maximum of the second element of each, and so on. (For example,
s1 and s2 might represent simultaneous rolls of two dice, and we want a
stream representing the maximum in each pair of rolls.) *)
let rec streammax (s1 : int stream) (s2 : int stream) : int stream =
  let Cons(a, b) = s1 () in
  let Cons(c, d) = s2 () in
    fun () -> Cons(max a c, streammax b d)
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
 * webcomic: http://www.xkcd.com/710/. *)

(* 2.7.1 *)
(* Write a function, collatz, that takes an integer and returns an int
 * stream with the integers that result from performing the above process.
 * Since streams must be infinite according to our definition, once (or if)
 * 1 is reached, the rest of the stream should be ones. *)
let even x = (x mod 2 = 0);;
let proc x = if x = 1 then 1
	     else (if even x then x / 2 else x * 3 + 1);;

let rec collatz (n : int) : int stream =
  fun () -> Cons(n, collatz (proc n))
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

type 'a str = Cons of 'a * 'a stream
and 'a stream = 'a str lazy_t 

let nats = lazy Cons(1, )

let head' (s: 'a stream) : 'a =
  let Cons(h, _) = Lazy.force s in h
;;

let tail' (s: 'a stream) : 'a stream =
  let Cons(_, t) = Lazy.force s in t
;;

let rec first' (n : int) (s : 'a stream) : 'a list = 
  match n with
  | 0 -> []
  | _ -> head' s :: first' (n -1) (tail' s)
;;

let map (f: 'a -> 'b) ('a stream

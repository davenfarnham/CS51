(* CS51 Section 4 *)

open Core.Std

(* ******************** Part 1 - Modules ************************* *)
(* The purpose of this section is to get you comfortable with the
 * syntax and concepts behind signatures and modules. You will gain
 * new respect for the abstraction barrier and how different
 * implementation choices can matter - but shouldn't affect the way
 * the module is used.
 *)

(* Modules are useful for a variety of things. One of these is the
 * naming problem. A code base that implements both stacks and queues
 * requires distinct functions that logically do the same thing but on
 * different types.
 *)

(** Stack Implementation **)
type 'a stack = 'a list (* a stack implemented as a list *)

let emp : 'a stack = []

(* what is a stack? *)
(* a stack is a data structure that's LIFO (last in, first out) *)

(* put something on the front, "top", of the stack *)
let put (st : 'a stack) (v : 'a) : 'a stack =
  v :: st
;;

(* pop something off the stack *)
let take (st : 'a stack) : ('a * 'a stack) option =
  match st with
  | [] -> None
  | hd :: tl -> (Some (hd, tl))
;;

(** Queue Implementation **)
type 'a queue = 'a list

let emp : 'a queue = []

(* what is a queue? *)
(* a queue is a data structure that's FIFO (first in, first out) *)

(* concat on to the back of the queue *)
let put (st : 'a queue) (v : 'a) : 'a queue =
  st @ [v]
;;

(* take off the front of the queue *)
let take (st : 'a queue) : ('a * 'a queue) option =
  match st with
  | [] -> None
  | hd :: tl -> (Some (hd, tl))
;;

(* Since both implementations use the same function names, we can't use them
 * both in the same code. We could solve this by renaming our functions,
 * for example change "put" to "stack_put" or "queue_put", but this is ugly
 * when we are only using one.
 *
 * We can solve this problem using modules. The primary motivation for modules
 * is to package together related definitions and enforce a consistent naming
 * scheme for these definitions. They wrap a set of definitions into their
 * own container or "namespace".
 *)

(** Stack Implementation **)
module Stack =
struct
  type 'a t = 'a list (* t here is just a generic name for a type. In this case, it stands in place of 'list' *)

  let emp : 'a t = [] (* function to return empty list *)

  let put (st : 'a t) (v : 'a) : 'a t =
    v :: st   
  ;;

  let take (st : 'a t) : ('a * 'a t) option =
    match st with
    | [] -> None
    | hd :: tl -> (Some (hd, tl))
  ;;
end

(** Queue Implementation **)
module Queue =
struct
  type 'a t = 'a list

  let emp : 'a t = []

  let put (st : 'a t) (v : 'a) : 'a t =
    st @ [v]
  ;;

  let take (st : 'a t) : ('a * 'a t) option =
    match st with
    | [] -> None
    | hd :: tl -> (Some (hd, tl))
  ;;
end

(* In OCaml, by convention, types are usually given the name "t" so we've
 * renamed the stack and queue types to "t".
 *
 * We can refer to values in the module using "dot-notation".
 *)

(** The empty queue **)
Queue.emp;;

(** The empty stack **)
Stack.emp;;

(** Adding values to a queue **)
Queue.put Queue.emp 5;;

(* We've successfully de-cluttered our namespace by factoring the code into
 * two modules, but the modules aren't providing us with anything beyond
 * this. For example, both stacks and queues are implemented using lists but
 * our modules are not hiding these facts. This means that OCaml will accept
 * bogus programs such as the following:
 *)

(* THE FOLLOWING IS VERY BAD *)

Queue.take (Stack.put Queue.emp 5);;

Queue.emp = Stack.emp;;

(* ******************** Part 2 - Signatures ************************* *)
(* To check these programs, OCaml is "unfolding" the type "'a t" and replacing
 * it with "'a list". This breaks abstraction because we might have invariants
 * on certain data structures that are not expressible in the OCaml type system.
 * For example, that a list should be sorted or should never be empty.
 *
 * One way to address this problem is to treat the type like a variable telling
 * OCaml, essentially:
 *
 *   There's this type "'a t", but I'm not going to tell you anything else
 *   about it.
 *
 * To achieve this in OCaml we use module signatures, which are analagous to
 * types in that, in classic SAT style:
 *
 *       'values' are to 'types' as
 *       'modules' are to 'signatures'
 *
 * We define signatures with the following OCaml syntax:
 *)

module type SCHEDULER =
sig
  type 'a t
  val emp : 'a t
  val put : 'a t -> 'a -> 'a t
  val take : 'a t -> ('a * 'a t) option
end

(* There are several things to note about this declaration in comparison to
 * modules that we defined above.
 * 1) We omit the definition of the type, i.e. the " = 'a list", so that OCaml
 *    must reason about that type abstractly.
 * 2) "let" declarations are changed to "val" declarations and only contain
 *    types.
 *
 * In the same way that we can assert types for values in OCaml, we can use the
 * ":" operator to assert that a module satisfies a signature.
 *)

(** Stack Implementation (is an implementation of the SCHEDULER signature) **)

module SStack: SCHEDULER = (* define SStack as a schedule - using the ":" just as you would other variables in function declarations *)
struct
  type 'a t = 'a list

  let emp : 'a t = [] 

  let put (st : 'a t) (v : 'a) : 'a t =
    v :: st
  ;;

  let take (st : 'a t) : ('a * 'a t) option =
    match st with
    | [] -> None
    | hd :: tl -> (Some (hd, tl))
  ;;
end

(* Or, you can just say:
module SStack = (Stack : SCHEDULER) (* Stack is the module that uses the signature defined by SCHEDULER *)
*)

(** Queue Implementation (is an implementation of the SCHEDULER signature) **)

module SQueue: SCHEDULER =
struct
  type 'a t = 'a list

  let emp : 'a t = []

  let put (st : 'a t) (v : 'a) : 'a t =
    st @ [v]
  ;;

  let take (st : 'a t) : ('a * 'a t) option =
    match st with
    | [] -> None
    | hd :: tl -> (Some (hd, tl))
  ;;
end

(* Or you can just say:
module SQueue = (Queue: SCHEDULER)
*)

(* Now that we have "sealed" the module, OCaml will treat the type "t"
 * abstractly. This prevents us from incorrectly using values of type "Stack.t"
 * in the Queue module and values of type "Queue.t" in the Stack module.
 *)

(* Stack.put Queue.emp 5;; *)
(* (\*> Error: This expression has type 'a Queue.t *)
(*  *>        but an expression was expected of type 'b Stack.t *)
(*  *\) *)

(* Queue.emp = Stack.emp;; *)
(* (\*> Error: This expression has type 'a Stack.t *)
(*  *>        but an expression was expected of type 'b Queue.t *)
(*  *\) *)

(* ******************** Part 3 - Functors ************************* *)
(* Signatures allow us to describe a module without needing to give a
 * precise "instantiation" of the signature. Continuing with the type
 * analogy, this is similar to our ability to describe all integers by
 * the type "int". This ability allows us to write functions over
 * modules. For more mathematical reasons, functions over modules are
 * called functors.
 *)

(* We're going to look at writing a simple functor that allows us to
 * traverse trees using a standard "work-list" algorithm. First we'll
 * consider the algorithm looking at some concrete instances.
 *
 *    [0]
 *   /  \
 * [1]  [2]
 *
 * The algorithm proceeds as follows:
 *
 *  visit node _n_:
 *    + add each child of n to the work-list
 *    + get the next node _m_ to visit from the work-list
 *    + visit _m_
 *
 *)
(* a tree is an abstract data type ADT that represents a hierarchical data structure of linked nodes *)
(* an ordered tree is simply one in which the children of a node are somehow ordered. A popular variant of this is
   the binary heap - 1) all levels except perhaps the last one will be completely filled and it will go left -> right 
   2) all nodes are either greater than or equal to or less than or equal to its children. *)
(* tree height - # of edges on the longest path from node to leaf; tree depth - root node has a depth of one*)
type tree =
  | Leaf
  | Branch of tree * int * tree

let traverse_df (tr : tree) : unit =
  let rec traverse_help (work_list : tree list) : unit =
    match work_list with
      | [] -> () (* what does unit do here? *)
      | Leaf :: rest -> traverse_help rest
      | Branch (l,v,r) :: rest ->
	let _ = print_string (string_of_int v ^ "\n") in (* added newline *)
	traverse_help (l :: r :: rest)
  in
  traverse_help [tr]
;;

(* simple tree example: tr = 

                   [2]    < min heap of our invariant (Strong, Even)
                  /   \
                 [3]  [5]
                /     /
              [4]    [6]  

tr = Branch(Branch(Branch(Leaf, 4, Leaf), 3, Leaf), 2, Branch(Branch(Leaf, 6, Leaf), 5, Leaf))
  -> 2
    -> 3
      -> 4
        -> 5
          -> 6

I'm going to go all the way down the left, then down the right
*)

let traverse_bf (tr : tree) : unit =
  let rec traverse_help (work_list: tree list) : unit = 
    match work_list with
    | [] -> ()
    | Leaf :: rest -> traverse_help rest
    | Branch (l,v,r) :: rest -> let _ = print_string (string_of_int v ^ "\n") in
				traverse_help (rest @ [l; r])
  in traverse_help [tr]					     
;;

(* using the same tree as above, we get:
  -> 2
    -> 3
      -> 5
        -> 4
          -> 6
*)

(* We can treat the work list abstractly. We don't care how it is implemented.
 * Therefore we can use a functor to abstract over the SCHEDULER implementation.
 *)
module Traverse =
  functor (S : SCHEDULER) -> (* apply the functor S to any Module with SIGNATURE SCHEDULER *)
struct
  let traverse (tr : tree) : unit =
    let rec traverse_help (work_list : tree S.t) : unit =
      match S.take work_list with
	| None -> ()
	| Some (Leaf,rest) -> traverse_help rest
	| Some (Branch (l,v,r), rest) ->
	  let _ = print_string (string_of_int v ^ "\n") in
	  traverse_help (S.put (S.put (rest) r) l) (* you put the l on second so it's on the top of the stack *)
    in
    traverse_help (S.put S.emp tr) (* start with the tree 'tr' *)
end


(* We can apply the functor to any module that has the signature SCHEDULER.
 * When we apply it to the stack we get a depth-first traversal.
 *)
module DepthFirstTraversal = Traverse (Stack) (* creating another module *)

(* We can also apply the functor to the Queue, since it also implements
 * the SCHEDULER signature. What kind of traversal does this give us?
 *)
module BreadthFirstTraversal = Traverse (Queue) (* will give you bf, but it won't be left to right *)

(* ***** Part 4 - Modules and Functors in the Standard Library **** *)
(* Now that we have seen how to write our own modules and functors we're
 * going to look at the conventions used for modules and functors in the
 * Core standard library.
 *
 * The Core standard library index can be found online at:
 * https://blogs.janestreet.com/ocaml-core/110.01.00/doc/core/#Std
 *
 * The standard library provides several data structures that are
 * built generically using modules and functors. For example, it
 * provides modules/functors for: finite sets, finite maps, stacks, and
 * queues.
 *
 * As a representative example, we're going to look at the set module.
 * Documentation:
 * https://blogs.janestreet.com/ocaml-core/110.01.00/doc/core/#Std.Set
 *)

(* Declare a finite set module over integers using the Core.Std library's
 * Set.Make functor.
 *)
module IntElt : Set.Elt with type t = int  =
struct
  type t = int
  let compare = compare

  (* For more on s-expressions, see
   * https://realworldocaml.org/v1/en/html/data-serialization-with-s-expressions.html
   *)
  let sexp_of_t = Int.sexp_of_t
  let t_of_sexp = Int.t_of_sexp
end

module IntSet = Set.Make (IntElt) (* functor Set.Make taking an IntElt *)

(* Now use your IntSet module to find the number of unique elements in a
 * list. Hint: Write a helper function that converts a list into an IntSet.
 *)

(* before we start, what is a set? It is a collection of distinct elements. *)
(* add elements to IntSet *)
let rec set_of_list (ls : int list) : IntSet.t =
  match ls with
  | [] -> IntSet.empty
  | hd :: tl -> (IntSet.add (set_of_list tl) hd) 
;;

(* unique then just becomes super easy *)
let rec count_unique (ls : int list) : int =
  IntSet.length (set_of_list ls)
;;

(* Now use your function to sum the unique elements in a list.
 *)
let sum_unique (ls : int list) : int =
  IntSet.fold_right ~f:(fun x y -> x + y) (set_of_list ls) ~init:0
;;

(* ******************** Part 5 - Binary Heaps ******************* *)
(* PS4 requires implementing a binary heap. Answer the following questions.
 *
 * What is a binary heap?
 * It's a tree data structure. We'll use it to create a priority queue in the pset. Every value of the node is 
 * less than every value of its descendents.
 *
 *
 * Why do we care about binary heaps?
 * We care about them because they're fast compared to other possible data structures.
 *
 *
 * What is an ODD tree? What is an EVEN tree?
 * An ODD tree is one where the left side has one more element than the right side
 * An EVEN tree is one where both sides have the same number of children
 *
 * What does it mean for a tree to be balanced?
 * A balanced tree is, generally, going to be one where the height of each subtree is equal or differs by
 * only one.
 *
 * What is the representation invariant for a binary heap?
 * It has to be balanced and every node has to be less than its descendents.
 *
 * Binary heaps are defined recursively, however they must respect a strict
 * invariant. How can we write recursive functions over binary heaps?
 * Carfeully. lol.
 *
 * How would you design the `mirror' function which swaps the left branch with
 * the right for a binary heap?
 * Switch branches.
 *
 *
 *
 *)

(* ******************** Part 6 - More Modules ************************* *)
(* We're now going to look at another module to get more comfortable
 * with abstract types.
 *
 * Consider the following signature for arbitrary sized integers:
 *)

module type BIGNAT =
sig
  type t
  val zero : t
  val is_zero : t -> bool

  (* add 1 *)
  val succ : t -> t

  (* minus 1 *)
  val pred : t -> t

  (* provide string representation of the integer *)
  val show : t -> string
end

(* One simple, though very inefficient, implementation of big integers is
 * natural numbers. Natural numbers can be defined inductively as:
 *
 *   A natural number is either:
 *     0 - "zero"
 *   -or-
 *     S n - "successor of n" (i.e. n + 1)
 *
 * Implement the Nat module using this inductive definition.
 *)

module Nat : BIGNAT =
struct
  type t = Z | S of t

  let zero : t =         ;;

  let is_zero (v : t) =          ;;

  let succ (v : t) : t =         ;;

  let pred (v : t) : t =



  ;;

  let show (v : t) : string =
    let rec to_int (v : t) (a : int) : int =
      match v with
      | Z -> a
      | S v -> to_int v (a + 1)
    in
    string_of_int (to_int v 0)
end

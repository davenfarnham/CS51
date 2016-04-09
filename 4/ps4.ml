(* PS4
 * Author: Daven Farnham
 * Partner: 
 *)

(* NOTE: Please read (and understand) all of the comments in this file! 
 * As well, COMMIT OFTEN! This pset can be pretty difficult in that it 
 * may stop compiling at points if you are approaching something in the wrong way. 
 * If that happens, you want to be able to get it back to a point where it compiled. 
 * COMMIT, COMMIT, COMMIT! *)

open Core.Std

(* Things related to the TreeQueue module in this file are commented out
 * because this file would not compile otherwise. Please uncomment them as you
 * get to them. *)

exception ImplementMe

type order = Equal | Less | Greater

(*****************************************************************************)
(*                              Part 1.5                                     *)
(*****************************************************************************)

(* Please read motivation.ml for some background on why we use functors here *)

(*****************************************************************************)
(*                               Part 2                                      *)
(*****************************************************************************)

(* A better signature for a binary tree, avoiding the comparison function
 * found in motivation.ml. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt

  (* What this type actually looks like is left up to the
   * particular BINTREE implementation (i.e. the struct) *)
  type tree

  (* Returns an empty tree *)
  val empty : tree

  (* Search a binary tree for the given value. *)
  val search : elt -> tree -> bool

  (* Insert elt into tree *)
  val insert : elt -> tree -> tree

  (* Delete the given value from a binary tree.
   * May raise NodeNotFound exception. *)
  val delete : elt -> tree -> tree

  (* Return the minimum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmin : tree -> elt

  (* Return the maximum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmax : tree -> elt

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* A signature for a module which defines a type and
 * how to compare values of that type, as well as ways of generating
 * values of that type. *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* See the testing.ml for an explanation of
   * what these "generate*" functions do, and why we included them in
   * this signature. *)

  (* Generate a value of type t *)
  val generate: unit -> t

  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t

  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t

  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  val generate_between: t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module IntCompare : COMPARABLE with type t=int =
struct
  type t = int

  let compare x y = if x < y then Less else if x > y then Greater else Equal

  let to_string = string_of_int

  let generate () = 0

  let generate_gt x () = x + 1

  let generate_lt x () = x - 1

  let generate_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* Another example implementation that only uses the first part
 * of the tuple in comparisons. *)
module IntStringCompare : COMPARABLE with type t=(int * string) =
struct
  type t = int * string
  let compare (p1,_) (p2,_) =
    if p1 < p2 then Less else if p1 > p2 then Greater else Equal

  let to_string (p, s) = "(" ^ string_of_int p ^ "," ^ s ^ ")"

  let () = Random.self_init ()

  let generate () = (0, string_of_int (Random.int Int.max_value))

  let generate_gt (p,s) () = (p+1, s)

  let generate_lt (p,s) () = (p-1, s)

  let generate_between (p1,_) (p2,s2) () =
    let (lower, higher) = (min p1 p2, max p1 p2) in
    (* Reuse the string from the second argument in the output value *)
    if higher - lower < 2 then None else Some (higher - 1, s2)
end

(* BinSTree is a *functor*, which takes an argument C which is a module
 * that implements the COMPARABLE signature. BinSTree ultimately
 * must return a module which matches the BINTREE signature.
 * We can do further abstraction by specifying a signature for
 * the functor, but won't do that here.
 *
 * Now that we are passing in a COMPARABLE module, which separately
 * defines a type and comparison for that type, we can just implement something
 * matching BINTREE's signature in terms of that type and comparison function,
 * and can wait until later to actually say what that type and comparison
 * function are.
 *
 * Here, you'll fill in the implementation of a binary search tree. Unlike a
 * usual binary search tree, this implementation keeps a list with each node in
 * the tree that contains each instance of the value inserted into the tree. For
 * example, if the integer 3 is inserted into an Int BinSTree 5 times, then
 * there will be a node with [3;3;3;3;3] in the tree, and the node will only be
 * removed after 5 deletions on 3 (assuming no further intermediate insertions
 * of 3).
 *)

module BinSTree(C : COMPARABLE) : BINTREE with type elt = C.t =
struct
  (* Inside of here, you can use C.t to refer to the type defined in
   * the C module (which matches the COMPARABLE signature), and
   * C.compare to access the function which compares elements of type
   * C.t
   *)
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in
   * this is the only place you explicitly need to use C.t; you
   * should use elt everywhere else *)
  type elt = C.t

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt list * tree

  (* Representation of the empty tree *)
  let empty = Leaf

  (* Avoid option returns from List.hd *)
  let head (lst : 'a list) : 'a = 
    match lst with
    | [] -> raise EmptyTree
    | hd :: _ -> hd

(*>* Problem 2.0 *>*)

  (* Define a method to insert element x into the tree t.
   * The left subtree of a given node should only have "smaller"
   * elements than that node, while the right subtree should only have
   * "greater". Remember that "equal" elements should all be stored in
   * a list. *The most recently inserted elements should be at the front
   * of the list* (this is important for later).
   *
   * Hint: use C.compare. See delete for inspiration
   *)
  let rec insert (x : elt) (t : tree) : tree = 
    match t with 
    | Leaf -> Branch (Leaf, [x], Leaf)
    | Branch (l, v, r) -> (match C.compare x (head v) with
			   | Less ->  Branch (insert x l, v, r)
			   | Greater -> Branch (l, v, insert x r)
			   | Equal -> Branch (l, x :: v, r))

(*>* Problem 2.1 *>*)

  (* Returns true if the element x is in tree t, else false *)
  (* Hint: multiple values might compare Equal to x, but
   * that doesn't necessarily mean that x itself is in the
   * tree.
   *)
  let rec search (x : elt) (t : tree) : bool = 
    match t with
    | Leaf -> false
    | Branch (l, v, r) -> (match C.compare x (head v) with
			   | Less -> search x l
			   | Greater -> search x r
			   | Equal -> true)


  (* A useful function for removing the node with the minimum value from
   * a binary tree, returning that node and the new tree.
   *
   * Notice that the pull_min function is not defined in the signature BINTREE.
   * When you're working on a structure that implements a signature like
   * BINTREE, you are free to write "helper" functions for your implementation
   * (such as pull_min) that are not defined in the signature.  Note, however,
   * that if a function foo IS defined in a signature BAR, and you attempt to
   * make a structure satisfying the signature BAR, then you MUST define the
   * function foo in your structure.  Otherwise the compiler will complain that
   * your structure does not, in fact, satisfy the signature BAR (but you claim
   * that it does).
   * So, if it's in the signature, it needs to be in the structure.  But if
   * it's in the structure, it doesn't necessarily need to show up in the
   * signature.
   *)
  let rec pull_min (t : tree) : elt list * tree =
    match t with
    | Leaf -> raise EmptyTree
    | Branch (Leaf, v, r) -> (v, r)
    | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))


  (* Removes an element from the tree. If multiple elements are in the list,
   * removes the one that was inserted first.  *)
  let rec delete (x : elt) (t : tree) : tree =
    match t with
    | Leaf -> raise NodeNotFound
    | Branch (l, lst, r) ->
      (* Reverse the list so that we pop off the last element in the list *)
      match List.rev lst with
      | [] -> failwith "Invalid tree: empty list as node"
      | hd::tl ->
        match C.compare x hd with
        | Less -> Branch (delete x l, lst, r)
        | Greater -> Branch (l, lst, delete x r)
        | Equal ->
          match tl with
          | _::_ -> Branch (l, List.rev tl, r)
          (* The list in the node is empty, so we have to
           * remove the node from the tree.  *)
          | [] ->
            match l, r with
            | Leaf, _ -> r
            | _, Leaf -> l
            | _ -> let v, r' = pull_min r in Branch (l,v,r')

(*>* Problem 2.2 *>*)

  (* Simply returns the minimum value of the tree t. If there are multiple
   * minimum values, it should return the one that was inserted first (note
   * that, even though the list might look like [3;3;3;3;3], you should
   * return the *last* 3 in the list. This is because we might pass in
   * a module to this functor that defines a type and comparison function
   * where each element in the list *is* distinct, but are Equal
   * from the perspective of the comparison function (like IntStringCompare).
   *
   * The exception "EmptyTree", defined within this module, might come in
   * handy. *)
  let getmin (t : tree) : elt = 
    let (e, _) = pull_min t in
      head (List.rev e)

(*>* Problem 2.3 *>*)

  (* Simply returns the maximum value of the tree t. Similarly should
   * return the last element in the matching list. *)
  let rec getmax (t : tree) : elt = 
    match t with
    | Leaf -> raise EmptyTree
    | Branch (_, v, Leaf) -> head (List.rev v)
    | Branch (_, _, r) -> getmax r  

  let test_insert () =
    let x = C.generate () in
    let t = insert x empty in
    assert (t = Branch(Leaf, [x], Leaf));
    let t = insert x t in
    assert (t = Branch(Leaf, [x;x], Leaf));
    let y = C.generate_gt x () in
    let t = insert y t in
    assert (t = Branch(Leaf, [x;x], Branch(Leaf, [y], Leaf)));
    let z = C.generate_lt x () in
    let t = insert z t in
    assert (t = Branch(Branch(Leaf, [z], Leaf),[x;x],
                Branch(Leaf, [y], Leaf)));
    (* Can add further cases here *)
    ()

  (* Insert a bunch of elements, and test to make sure that we
   * can search for all of them. *)
  let test_search () =
    let x = C.generate () in
    let t = insert x empty in
    assert (search x t);
    let order = [true; false; true; true; true; false; false] in
    let full_tree, values_inserted =
      List.fold_right
        ~f:(fun current_order (tree_so_far, values_so_far) ->
          let prev_value =
            match values_so_far with
            | [] -> x
            | hd :: _ -> hd
          in
          let value =
            if current_order
            then C.generate_gt prev_value ()
            else C.generate_lt prev_value ()
          in
          insert value tree_so_far, value :: values_so_far
        ) ~init:(t, []) order
    in
    List.iter ~f:(fun value -> assert (search value full_tree)) values_inserted

  (* None of these tests are particularly exhaustive.
   * For instance, we could try varying the order in which we insert
   * values, and making sure that the result is still correct.
   * So, the strategy here is more to try to build up a reasonable degree
   * of coverage across the various code-paths, rather than it is to
   * test exhaustively that our code does the right thing on every single
   * possible input. *)
  let test_getmax () =
    let x = C.generate () in
    let x2 = C.generate_lt x () in
    let x3 = C.generate_lt x2 () in
    let x4 = C.generate_lt x3 () in
    assert (getmax (insert x4 (insert x3 (insert x2 (insert x empty)))) = x)

  let test_getmin () =
    let x = C.generate () in
    let x2 = C.generate_gt x () in
    let x3 = C.generate_gt x2 () in
    let x4 = C.generate_gt x3 () in
    assert (getmin (insert x2 (insert x4 (insert x (insert x3 empty)))) = x)

  let test_delete () =
    let x = C.generate () in
    let x2 = C.generate_lt x () in
    let x3 = C.generate_lt x2 () in
    let x4 = C.generate_lt x3 () in
    let after_ins = insert x4 (insert x3 (insert x2 (insert x empty))) in
    assert (delete x (delete x4 (delete x3 (delete x2 after_ins))) = empty)

  let run_tests () =
    test_insert ();
    test_search ();
    test_getmax ();
    test_getmin ();
    test_delete ();
    ()

end

(* Here is how you would define an int binary tree using the BinSTree
 * functor, which expects a module to be passed in as an argument.
 * You should write tests using the IntTree module (or you can
 * give the module a different type), and you should use
 * this call to a functor as an example for how to test modules further
 * down in the pset.
 *)

module IntTree = BinSTree(IntCompare)

(* Please read the entirety of "testing.ml" for an explanation of how
 * testing works.
 *)
let _ = IntTree.run_tests ()

(*****************************************************************************)
(*                               Part 3                                      *)
(*****************************************************************************)

(* A signature for a priority queue. See the pset specification on the
 * course website and section notes for week 4
 * for more information if you are unfamiliar with priority queues.
 *
 * IMPORTANT: In your implementations of priority queues, the MINIMUM
 * valued element corresponds to the HIGHEST priority. For example,
 * in just an int prioqueue, the integer 4 has lower priority than
 * the integer 2.
 *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(*>* Problem 3.0 *>*)

(* Implement a priority queue using lists
 * You can use OCaml's built-in lists (i.e. [] and ::)
 * You are also free to use anything from the List module,
 *)
module ListQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct
  (* Remember to use the "C" (COMPARABLE) module! You may want to
   * look above at BinSTree for inspiration *)
  exception QueueEmpty

  type elt = C.t

  type queue = elt list

(*>* Problem 3.1 *>*)
  let empty = []

(*>* Problem 3.2 *>*)
  let is_empty (t : queue) = 
    match t with 
    | [] -> true
    | _ -> false

(*>* Problem 3.3 *>*)

  (* Like with getmin and getmax in your binary tree, you should implement
   * add and take such that, if all elements have the same priority, this
   * module simply becomes a regular queue (i.e., elements inserted earlier
   * should be removed before elements of the same priority inserted later)
   *)
  let rec add (e : elt) (q : queue) =
    match q with
    | [] -> [e] 
    | hd :: tl -> match C.compare e hd with
		  | Less -> (e :: q)
		  | Equal | Greater -> (hd :: (add e tl))

(*>* Problem 3.4 *>*)
  let take (q : queue) : (elt * queue) = 
    match q with
    | [] -> raise QueueEmpty
    | hd :: tl -> (hd, tl) (* O(1) *)

  let test_empty_and_take_add () =
    assert(empty = []);
    let x = C.generate () in
    let x' = C.generate () in
    let x1 = C.generate_gt x () in
    let x0 = C.generate_lt x () in
    let q = add x empty in
      let q1 = add x0 q in
        let q2 = add x1 q1 in
	  let q3 = add x' q2 in
            let (v, r) = take q3 in
              assert(v = x0);
	    let (v', _) = take r in
	      assert (v' = x) 

  let run_tests () = 
    test_empty_and_take_add ()

end

module IntQueue = ListQueue(IntCompare)
let _ = IntQueue.run_tests ()


(*>* Problem 3.5 *>*)

(* Now implement a priority queue using a Binary Search Tree.
 * Luckily, you should be able to use *a lot* of your code from above! *)

module TreeQueue(C : COMPARABLE) : PRIOQUEUE with type elt = C.t=
struct
  exception QueueEmpty

  (* You can use the module T to access the functions defined in BinSTree,
   * e.g. T.insert *)
  module T = (BinSTree(C) : (BINTREE with type elt = C.t))

  type elt = C.t

  type queue = T.tree

  let empty = T.empty

  let is_empty (t : queue) = 
    if t = empty then true else false

  let add (e : elt) (q : queue) =
    T.insert e q

  let take (q : queue) = 
    let min = T.getmin q in 
      let q' = T.delete min q in
        (min, q')

  let test_empty_and_take_add () =
    let x = C.generate () in
    let x1 = C.generate_gt x () in
    let x0 = C.generate_lt x () in
    let q = add x empty in
      let q1 = add x0 q in
        let q2 = add x1 q1 in
          let (v, q3) = take q2 in
            assert(v = x0);
	    assert (q3 = (add x1 (add x empty))) 

  let sort_test () = 
    let x5 = C.generate () in
    let x4 = C.generate_lt x5 () in 
    let x3 = C.generate_lt x4 () in
    let x2 = C.generate_lt x3 () in
    let x = C.generate_lt x2 () in
      let q = add x5 empty in
       let q1 = add x4 q in
         let q2 = add x2 q1 in
           let q3 = add x q2 in
             let q4 = add x3 q3 in
	       let (v, w) = take q4 in
                 assert (v = x);	       
               let (v', w') = take w in
		 assert (v' = x2);		
               let (v'', w'') = take w' in
		 assert (v'' = x3);		
               let (v''', w''') = take w'' in
		 assert (v''' = x4);		
               let (v'''', _) = take w''' in
		 assert (v'''' = x5)

  let run_tests () = 
    test_empty_and_take_add ();
    sort_test ()

end

(* compilation seems to throw an error regarding Random module in IntString *)
module ITreeQueue = TreeQueue(IntCompare)
let _ = ITreeQueue.run_tests ()


(*****************************************************************************)
(*                               Part 4                                      *)
(*****************************************************************************)

(*>* Problem 4.0 *>*)

(* Now for the good stuff :-) Implement a priority queue using a binary heap.
 * See the pset spec for more info.
 *
 * You should implement a min-heap, i.e. the top of your heap stores the
 * smallest element in the entire heap.
 *
 * Note that, unlike for your tree and list implementations of priority queues,
 * you do *not* need to worry about the order in which elements of equal
 * priority are removed. Yes, this means it's not really a "queue", but
 * it is easier to implement without that restriction.
 *)
module BinaryHeap(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty
  exception Error

  type elt = C.t

  (* Be sure to read the pset spec for hints and clarifications.
   *
   * Remember the invariants of the tree that make up your queue:
   * 1) A tree is ODD if its left subtree has 1 more node than its right
   * subtree. It is EVEN if its left and right subtrees have the same number of
   * nodes. The tree can never be in any other state. This is the WEAK
   * invariant, and should never be false.
   *
   * 2) All nodes in the subtrees of a node should be *greater* than (or equal
   * to) the value of that node. This, combined with the previous invariant,
   * makes a STRONG invariant. Any tree that a user passes in to your module
   * and receives back from it should satisfy this invariant.  However, in the
   * process of, say, adding a node to the tree, the tree may intermittently
   * not satisfy the order invariant. If so, you *must* fix the tree before
   * returning it to the user.  Fill in the rest of the module below!
   *)
  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* A tree is either just a single element, has one branch (the first elt in
   * the tuple is the element at this node, and the second elt is the element
   * down the branch), or has two branches (with the node being even or odd) *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) = q = Empty

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    (* Given a tree, where e will be inserted is deterministic based on the
     * invariants. If we encounter a node in the tree where its value is greater
     * than the element being inserted, then we place the new elt in that spot
     * and propagate what used to be at that spot down toward where the new
     * element would have been inserted *)
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* Simply returns the top element of the tree t (i.e., just a single pattern
   * match in *)
  let get_top (t : tree) : elt = 
    match t with
    | Leaf e -> e
    | OneBranch(e, _) -> e
    | TwoBranch(_, e, _, _) -> e

  (* return elt < e plus new tree with e swapped for elt *)
  let rec find (e: elt) (t: tree) : (elt * tree) option = 
    match t with
    | Leaf e1 -> (match C.compare e e1 with
		  | Less | Equal -> None
		  | Greater -> Some (e1, Leaf e))
     
    | OneBranch (e1, e2) -> (match C.compare e e1 with
			     | Less | Equal -> None
			     | Greater -> (match C.compare e e2 with
					   | Less | Equal -> Some (e1, OneBranch (e, e2))
					   | Greater -> Some (e1, OneBranch (e2, e))))

    | TwoBranch (b, e1, t1, t2) -> (match C.compare e e1 with
				    | Less | Equal -> (match find e t1 with
						       | None -> (match find e t2 with
							          | None -> None
							          | Some (e', t') -> Some (e', TwoBranch (b, e1, t1, t')))
						       | Some (e', t') -> Some (e', TwoBranch (b, e1, t', t2)))
  				    | Greater -> Some (e1, TwoBranch (b, e, t1, t2)))

  (* Takes a tree, and if the top node is greater than its children, fixes
   * it. If fixing it results in a subtree where the node is greater than its
   * children, then you must (recursively) fix this tree too. *)
  let rec fix (t : tree) : tree = 
    match t with
    | Leaf _ -> t
    | OneBranch (e1, e2) -> (match C.compare e1 e2 with
			     | Less | Equal -> t
			     | Greater -> OneBranch (e2, e1))
    | TwoBranch (b, e, t1, t2) -> let e1 = get_top t1 in 
				    let e2 = get_top t2 in
				      match C.compare e1 e2 with
				      | Less -> (match find e t1 with
						 | None -> t
						 | Some (e', t') -> TwoBranch (b, e', fix t', t2))
				      | Greater | Equal -> (match find e t2 with
							    | None -> t
						 	    | Some (e', t') -> TwoBranch (b, e', t1, fix t'))
								
  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t -> t

  (* Takes a tree, and returns the item that was most recently inserted into
   * that tree, as well as the queue that results from removing that element.
   * Notice that a queue is returned (since removing an element from just a leaf
   * would result in an empty case, which is captured by the queue type
   *
   * By "item most recently inserted", we don't mean the
   * most recently inserted *value*, but rather the newest node that was
   * added to the bottom-level of the tree. If you follow the implementation
   * of add carefully, you'll see that the newest value may end up somewhere
   * in the middle of the tree, but there is always *some* value brought
   * down into a new node at the bottom of the tree. *This* is the node
   * that we want you to return.
   *)
  let rec get_last (t : tree) : elt * queue = 
    match t with
    | Leaf e -> (e, Empty)
    | OneBranch (e1, e2) -> (e2, Tree (Leaf e1))
    | TwoBranch (Odd, e, t1, t2) -> (match get_last t1 with
				     | (_, Empty) -> raise Error
				     | (e', Tree t') -> (e', Tree (TwoBranch (Even, e, t', t2))))
    | TwoBranch (Even, e, t1, t2) -> (match get_last t2 with
				      | (e', Empty) -> (e', Tree (OneBranch(e, get_top t1))) 
				      | (e', Tree t') -> (e', Tree (TwoBranch (Odd, e, t1, t'))))

  (* Implements the algorithm described in the writeup. You must finish this
   * implementation, as well as the implementations of get_last and fix, which
   * take uses *)
  let take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    (* Implement the odd case! *)
    | TwoBranch (Odd, e, t1, t2) -> 
      let (last, q2') = get_last t1 in
      (match q2' with
       | Tree t1' -> (e, Tree (fix (TwoBranch (Even, last, t1', t2))))
       | _ -> raise Error)      

  let test_get_last () = 
    let t = empty in 
      let x = C.generate () in (* 5 *)
    	let x1 = C.generate_gt x () in (* 7 *)
    	  let x2 = C.generate_gt x1 () in (* 9 *)
     	    let x3 = C.generate_gt x2 () in (* 15 *)
    	      let x4 = C.generate_gt x3 () in (* 17 *)
    	        let x5 = C.generate_gt x4 () in (* 25 *)
		    let t1 = add x t in 
		    let t2 = add x2 t1 in 
		    let t3 = add x1 t2 in 
		    let t4 = add x4 t3 in 
		    let t5 = add x3 t4 in 
		    let t6 = add x5 t5 in 
                      match t6 with
                      | Empty -> raise Error
                      | Tree t' -> let (e, q) = get_last t' in
		     	             assert (e = x5);
			             assert (q = Tree (TwoBranch (Even, x, OneBranch (x2, x4), OneBranch(x1, x3))));
				     match q with
				     | Empty -> raise Error
				     | Tree t'' -> let (e', q') = get_last t'' in
				       		     assert (e' = x3);
				       		     assert (q' = Tree (TwoBranch (Odd, x, OneBranch (x2, x4), Leaf x1)));
				     match t1 with
				     | Empty -> raise Error
				     | Tree t1' -> let (e'', q'') = get_last t1' in
						     assert (e'' = x);
	                                             assert (q'' = Empty)

  let test_take () = 
    let t = empty in 
      let x = C.generate () in (* 5 *)
    	let x1 = C.generate_gt x () in (* 7 *)
    	  let x2 = C.generate_gt x1 () in (* 9 *)
     	    let x3 = C.generate_gt x2 () in (* 15 *)
    	      let x4 = C.generate_gt x3 () in (* 17 *)
    	        let x5 = C.generate_gt x4 () in (* 25 *)
		  let t1 = add x t in 
		  let t2 = add x2 t1 in 
		  let t3 = add x1 t2 in 
		  let t4 = add x4 t3 in 
		  let t5 = add x3 t4 in 
                  let t6 = add x5 t5 in 
		    (* test taking from an ODD TwoBranch *)
	            let (e, q) = take t6 in
		      assert (e = x);
		      assert (q = Tree (TwoBranch (Even, x1, OneBranch (x2, x4), OneBranch (x3, x5))));
 		      (* test taking from an EVEN TwoBranch *)
		      let (e', q') = take q in
			assert (e' = x1);
			assert (q' = Tree (TwoBranch (Odd, x2, OneBranch (x4, x5), Leaf x3)));
		  let x6 = C.generate_lt x () in
		    let t7 = add x6 t6 in
		      let (e'', q'') = take t7 in
		        assert (e'' = x6);
			assert (q'' = Tree (TwoBranch (Odd, x, TwoBranch (Even, x2, Leaf x4, Leaf x5), OneBranch (x1, x3))))

  let run_tests () = 
    test_get_last ();
    test_take ()

end


(* Now to actually use our priority queue implementations for something useful!
 *
 * Priority queues are very closely related to sorts. Remember that removal of
 * elements from priority queues removes elements in highest priority to lowest
 * priority order. So, if your priority for an element is directly related to
 * the value of the element, then you should be able to come up with a simple
 * way to use a priority queue for sorting...
 *
 * In OCaml 3.12, modules can be turned into first-class
 * values, and so can be passed to functions! Here, we're using that to avoid
 * having to create a functor for sort. Creating the appropriate functor
 * is a challenge problem :-)
 *)

(* The following code is simply using our functors and passing in a
 * COMPARABLE module for integers, resulting in priority queues
 * tailored for ints
 *)
module IntListQueue = (ListQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntHeapQueue = (BinaryHeap(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)
module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* test functions in binary heap *)
let _ = IntHeapQueue.run_tests ()

(* store the whole modules in these variables *)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = IntCompare.t)
let tree_module = (module IntTreeQueue : PRIOQUEUE with type elt = IntCompare.t)

(* Implements sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in

  let rec extractor pq lst =
    if P.is_empty pq then lst else
    let (x, pq') = P.take pq in
    extractor pq' (x::lst) in
  let pq = List.fold_right ~f:P.add ~init:P.empty lst in
  List.rev (extractor pq [])

(* Hurray!! Now, we can pass in the modules into sort and get out
 * different sorts!! *)

(* Sorting with a priority queue with an underlying heap
 * implementation is equivalent to heap sort! *)
let heapsort = sort heap_module

let _ = assert (heapsort [] = [])
let _ = assert (heapsort [5;4;2;1;3] = [1;2;3;4;5])
let _ = assert (heapsort [5;4;3;2;1] = [1;2;3;4;5])
let _ = assert (heapsort [-5;-4;2;1;3] = [-5;-4;1;2;3])


(* Sorting with a priority queue with your underlying tree
 * implementation is *almost* equivalent to treesort;
 * a real treesort relies on self-balancing binary search trees *)

let treesort = sort tree_module

(* to debut treesort *)
let rec print_list (l: int list) : unit = 
  match l with
  | [] -> print_string "\n"
  | hd :: tl -> print_int hd; print_list tl
;;

let _ = assert (treesort [] = [])
let _ = assert (treesort [5;4;2;1;3] = [1;2;3;4;5])
let _ = assert (treesort [5;4;3;2;1] = [1;2;3;4;5])
let _ = assert (treesort [-5;-4;2;1;3] = [-5;-4;1;2;3])


(* Sorting with a priority queue with an underlying unordered list
 * implementation is equivalent to heap sort! If your implementation of
 * ListQueue used ordered lists, then this is really insertion sort *)
let selectionsort = sort list_module

let _ = assert (selectionsort [] = [])
let _ = assert (selectionsort [5;4;2;1;3] = [1;2;3;4;5])
let _ = assert (selectionsort [5;4;3;2;1] = [1;2;3;4;5])
let _ = assert (selectionsort [-5;-4;2;1;3] = [-5;-4;1;2;3])


(* You should test that these sorts all correctly work, and that
 * lists are returned in non-decreasing order!! *)


(*****************************************************************************)
(*                               Part N                                      *)
(*****************************************************************************)

(*>* Problem N.0 *>*)
(* *Highly recommended (and easy)* Challenge problem:
 * Above, we only allow for sorting on int lists. Write a functor that will take
 * a COMPARABLE module as an argument, and allows for sorting on the
 * type defined by that module. You should use your BinaryHeap module.
 *)

(*>* Problem N.1 *>*)
(* Challenge problem:
 * Now that you are learning about asymptotic complexity, try to
 * write some functions to analyze the running time of
 * the three different sorts. Record in a comment here the results of
 * running each type of sort on lists of various sizes (you may find
 * it useful to make a function to generate large lists).
 * Of course include your code for how you performed the measurements below.
 * Be convincing when establishing the algorithmic complexity of each sort.
 * See the Sys module for functions related to keeping track of time *)

(*>* Problem N.2 *>*)
let minutes_spent : int = 51

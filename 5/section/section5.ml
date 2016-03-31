(*** SECTION 5 ***)

(* Goal: Get you ready for Moogle
 *
 * 1. Practice writing and using modules and functors
 * 2. Walk through A couple data types: sets, graphs, dictionaries
 * 3. An overview of Moogle and some tips
 *)

open Core.Std;;

(****************************** Helpers *********************************)

(* The type Ordering.t from the Core library is used for comparison operations *)
(* type Ordering.t = Less | Equal | Greater ;; *) (* Ordering.t simply allows us to use these words *)

module type NODE =
sig
  type node
  val compare : node -> node -> Ordering.t
end
;;

(****************** Part 1: Module Fun with Sets  ********************)

(* What is the difference between sets and lists? *)

(* Wiki: "A set is an abstract data structure that can store certain values,
 * without any particular order, and no repeated values. It is a computer
 * implementation of the mathematical concept of a finite set." *)

(* Here is the signature for a set of nodes *)

module type NODESET =
sig
  module N : NODE
  type node = N.node
  type nodeset
  val empty : nodeset
  val isempty : nodeset -> bool
  (*  We cannot depend upon choose to return a specific element--
   *  the selection is random. If the set is empty, use None.*)
  val choose : nodeset -> (node * nodeset) option
  (*  Add a node to the set, but if node is repeated, do nothing. *)
  val put : node -> nodeset -> nodeset
end
;;

(******************** Part 4: Dictionaries  *********************)

(* What is a dictionary?
 *
 * Wiki: "a collection of unique keys and a collection of values, where each
 * key is associated with one value."
 *
 * Examples: an actual dictionary, phonebook.
 *)

(* Problem 4.1
 * Write a signature for a dictionary. What would you include?
 *)

module type DICT =
sig
  type key
  type value
  type dict
  val empty : dict
  val insert : dict -> key -> value -> dict
  val lookup : dict -> key -> value option
  val remove : dict -> key -> dict
  val member : dict -> key -> bool
  val choose : dict -> (key * value * dict) option
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
end

(* Problem 4.2
 * What are some ways to implement a dictionary? No need to write code.
 *)

(* EXAMPLE SOLUTION:
   type dict = (key * value) list;;
   type dict = [key; value] list;; (* nope, not same types! *)
   type dict = (key * value) queue / stack;;
   type dict = (key * value) tree;;
   using Map library in OCaml
   etc.
*)

(* Problem 4.3
 * How can we write set in terms of dictionary? No need to write code.
 *)

(*
   (* remember, you're not reinventing everything in the crawler, for example, you'll still have
      your dictionary but this dictionary will be pointing to a set that is implemented as a dictionary -
      have the value in set-dictionary be nothing (unit) since the key here is really just the original dictionary's
      value. *)

   module DictSet(D : Dict.DICT with type value = unit) 
   : (SET with type elt = D.key) =
   struct
   ...
   end
*)

(* Problem 4.4
 * How can we write graph in terms of dictionary? No need to write code.
 *)

(* EXAMPLE SOLUTION:
   We can represent a graph as an edge dictionary:
   dictionary: node -> neighbor set
   Every node in the graph must be a key in the dictionary.
*)

(****************** Part 5: Getting started with Moogle *******************)

(* Moogle walk through.
 * moogle.ml is the main file -- Start at the bottom and walk through the
 * calls there. Point out that the query engine is in many ways very similar to
 * the expression evaluation they did in PS2.
 *)

(* Discussion:
 * Question 5.1: What's a good strategy for tackling a "large" codebase?
 * - Look over all the files.  Look at signatures and types, not
 * implementations.
 * - Figure out what are the abstractions being used.
 * - Draw a picture of the main pieces, and what depends on what?

 * Question 5.2: How do I debug a "large" codebase?
 * - Unfortunately, we're too big for toplevel now.
 * - Use printing, asserts.
 * - Write a test function which they should call on startup to test
 * their code and print out their sets.  (Invoke your test as part
 * of startup, and then use the command-line tools for compiling your
 * code instead of the interactive loop.)
 *)

(*************************** Part 2: 2-3 Trees ****************************)

(* Your implementation of binary search trees from problem set 4 was relatively
 * straightforward, but unfortunately it doesn't always have the ideal
 * O(log(n)) insertion, deletion, and lookup times.
 *
 * Problem 2.1 What is an example of a series of insertions into a tree that
 * would cause the tree to be "unbalanced"?
 *
 * Answer:
 * Inserting increasingly large elements into the tree results in a tree which
 * is fundamentally just a linked list (e.g. 1,2,3,4,5,6...).
 *
 * In Moogle, we will have you implement 2-3 trees, which is just one
 * example of a balanced search tree (hopefully you read the pdf
 * that we asked you to before today!). A 2-3 tree is just one way in
 * which you might implement a dictionary; we already implemented
 * a list version for you. So, a dictionary is a good example of a *signature*,
 * for which you will implement a 2-3 tree *functor* whose output must match
 * that signature.
 *
 * Here is the type for a 2-3 tree:
 *)
type pair = key * value (* sorting based on the key *)
type dict =
  | Leaf
  | Two of dict * pair * dict
  | Three of dict * pair * dict * pair * dict

(* INVARIANTS:
 * 2-node: Two(left,(k1,v1),right)
 * (1) Every key k appearing in subtree left must be k < k1.
 * (2) Every key k appearing in subtree right must be k > k1.
 * (3) The length of the path from the 2-node to
 *     every leaf in its two subtrees must be the same.
 *
 * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
 * (1) k1 < k2.
 * (2) Every key k appearing in subtree left must be k < k1.
 * (3) Every key k appearing in subtree right must be k > k2.
 * (4) Every key k appearing in subtree middle must be k1 < k < k2.
 * (5) The length of the path from the 3-node to every leaf in its three
 *     subtrees must be the same.
 *
 *
 * See the 2-3 tree pdf from the problem set spec for examples of insertions
 * and deletions into the 2-3 tree.
 *)

(**************************** Part 3: Graphs ******************************)

(* What is a graph?
 *
 * Wikipedia: "A graph is an abstract representation of a set of objects where
 * some pairs of the objects are connected by links. The interconnected objects
 * are represented by mathematical abstractions called vertices [or nodes], and
 * the links that connect some pairs of vertices are called edges. Typically, a
 * graph is depicted in diagrammatic form as a set of dots for the vertices,
 * joined by lines or curves for the edges."
 *
 * Two kinds of graphs: Directed vs. undirected. Examples: Facebook's
 * social graph is a undirected graph. Google's (or Moogle's) webpage
 * link graph is directed.
 *)

module type GRAPH =
  sig
    module N : NODE
    type node = N.node
    type graph
    val empty : graph
    val isempty : graph -> bool
    val neighbors : graph -> node -> node list
    val choose : graph -> node option
    val remove : graph -> node -> graph
  end
;;

(* Here is one implementation. *)

module UndirectedGraph (NA: NODE) : GRAPH =
struct
  module N = NA
  type node = N.node

  (* Problem 3.1
   * How is this type being used to represent graphs
   * (note that it is frequently called an adjacency-list representation)
   *
   * Answer:
   * The first node in the tuple is each node that appears in the graph.
   * The node list is each node that the first node in the tuple is
   * neighbors with (shares an edge with). There are a couple of invariants
   * here:
   * 1) If a node appears in one of the neighbor node lists, it must appear
   *    somewhere in the first position in the tuple.
   * 2) Since it is an undirected graph, if A's node list has B in it, then
   *    B's node list must have A (they are mutual neighbors).
   *)
  type graph = (node * node list) list

  exception EmptyGraph
  exception IllFormedGraph

  (* remove the duplicates in the graph *)
  let remove_dupls (l:'a list) : 'a list =
    List.fold_right ~f:(fun x r -> if List.mem r x then r else x::r) ~init:[] l ;;

  let empty : graph = [] ;;

  let isempty (g:graph) : bool =
    match g with
      | [] -> true
      | _ -> false ;;

  (* find all the neighbors in the graph of node n *)
  let neighbors (g:graph) (n:node) : node list =
    match List.filter ~f:(fun (n',_) -> n' = n) g with
      | [] -> raise EmptyGraph
      | [(_,nbrs)] -> nbrs (* nbrs, remember, is a node list inside this tuple *)
      | _ :: _ :: _ -> raise IllFormedGraph ;;

  (* choose a node [vertex] from the graph *)
  let choose (g:graph) : node option =
    match g with
      | [] -> None
      | (n,_) :: _ -> Some n ;;

  (* remove node n from the graph *)
  let rec remove (g:graph) (n:node) : graph =
    match g with
      | [] -> []
      | (n',nbrs)::g' ->
        if n' = n then remove g' n
        else (n', List.filter ~f:(fun x -> x <> n) nbrs) :: remove g' n ;; (* this removes the node n from the neighbors *) 
                                                                           (* the function in the filter says, if x != n, include it 
                                                                              in the neighbors list, otherwise, remove it, then call remove on the
                                                                              rest of the graph *) 
end
;;

(* Problem 3.2
 * How would we have to modify our breadth-first and depth-first searches on
 * trees from last week to now work on graphs?
 * Answer: We must now be sure that we don't visit the same node twice, and
 * that we don't add the same child to our "work-list" twice. The first
 * can be accomplished by keeping a list of which nodes we've already visited.
 * The second can be accomplished by using a "work-set" instead of a work-list.
 *)

(* Problem 3.3
 *
 * In what order would a BFS visit the nodes in the graph below, starting
 * at 1? What about a DFS? (Assume the numerically lower
 * child is always added to the work-set first)
 *
 *          1--2--10--11
 *         /    \    /
 *        3      4--5
 *       / \    /   |
 *       6--7--8    9--14
 *         / \
 *        12  13
 *)

(*** SOLUTION ***
     * DFS: 1, 2, 4, 5, 9, 14, 11, 10, 8, 7, 3, 6, 12, 13
     * BFS: 1, 2, 3, 4, 10, 6, 7, 5, 8, 11, 12, 13, 9, 14
*)



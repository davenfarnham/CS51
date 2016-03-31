(*** SECTION 5 ***)

(* Goal: Get you ready for Moogle
 *
 * 1. Practice writing and using modules and functors
 * 2. Walk through A couple data types: sets, graphs, dictionaries
 * 3. An overview of Moogle and some tips
 *)

open Core.Std;;

(* The type Ordering.t from the Core library is used for comparison operations *)
(* type Ordering.t = Less | Equal | Greater ;; *)

module type NODE =
sig
  type node
  val compare : node -> node -> Ordering.t
end
;;

(**************************** Part 1: Sets *******************************)

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
    (* you can't depend on choose to return a specific node - you'll get something back,
       but you won't know exactly what. *)
    val choose : nodeset -> (node * nodeset) option
    (* How does put differ from, say, "add" when you did priority queues? *)
    (* If the node is already in the nodeset, it won't put the node again *)
    val put : node -> nodeset -> nodeset
  end
;;

(******************** Part 4: Dictionaries  *********************)

(* What is a dictionary?
 *
 * Wiki: "a collection of unique keys and a collection of values, where each
 * key is associated with one value (this value can be a set, however)."
 *
 * Examples: an actual dictionary, phonebook.
 *)

(* Problem 4.1
 * Write a signature for a dictionary. What would you include?
 *)

module type DICT =
sig

end

(* Problem 4.2
 * What are some ways to implement a dictionary? No need to write code.
 *)







(* Problem 4.3
 * How can we write set in terms of dictionary? No need to write code.
 *)






(* Problem 4.4
 * How can we write graph in terms of dictionary? No need to write code.
 *)







(****************** Part 5: Getting started with Moogle *******************)

(* Moogle walk through. *)

(* Discussion:
 * Question 5.1: What's a good strategy for tackling a "large" codebase?







 * Question 5.2: How do I debug a "large" codebase?







 *)

(*************************** Part 2: 2-3 Trees ****************************)

(* Your implementation of binary search trees from problem set 4 was relatively
 * straightforward, but unfortunately it doesn't always have the ideal
 * O(log(n)) insertion, deletion, and lookup times.
 *
 * Problem 2.1 What is an example of a series of insertions into a tree that
 * would cause the tree to be "unbalanced"?
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
type pair = key * value
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
 * Two kinds of graphs: Directed vs. undirected. Examples: Facebook's social
 * graph is a undirected graph. Google's (or Moogle's) page rank is directed.
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
     *)
    type graph = (node * node list) list

    exception EmptyGraph
    exception IllFormedGraph

    let remove_dupls (l:'a list) : 'a list =
      List.fold_right ~f:(fun x r -> if List.mem r x then r else x::r) ~init:[] l ;;

    let empty : graph = [] ;;

    let isempty (g:graph) : bool =
      match g with
        | [] -> true
        | _ -> false ;;

    let neighbors (g:graph) (n:node) : node list =
      match List.filter ~f:(fun (n',_) -> n' = n) g with
        | [] -> raise EmptyGraph
        | [(_,nbrs)] -> nbrs
        | _ :: _ :: _ -> raise IllFormedGraph ;;

    let choose (g:graph) : node option =
      match g with
        | [] -> None
        | (n,_) :: _ -> Some n ;;

    let rec remove (g:graph) (n:node) : graph =
      match g with
        | [] -> []
        | (n',nbrs)::g' ->
            if n' = n then remove g' n
            else (n', List.filter ~f:(fun x -> x <> n) nbrs) :: remove g' n ;;
  end
;;

(* Problem 3.2
 * How would we have to modify our breadth-first and depth-first searches on
 * trees from last week to now work on graphs?
 *)

(* Problem 3.3
 *
 * In what order would a BFS visit the nodes in the graph below, starting
 * at 1? What about a DFS?
 *
 *          1--2--10--11
 *         /    \    /
 *        3      4--5
 *       / \    /   |
 *       6--7--8    9--14
 *         / \
 *        12  13
 *)






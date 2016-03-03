(*****************************************************************************)
(*                              Part 1.5                                     *)
(*****************************************************************************)

(** Problem 1: quick intro to modules; no points, but please read!! **)

(* A type useful in comparing two values of the same type *)
type order = Equal | Less | Greater

(* Here's is a possible signature ("interface") for a binary tree.
 * Notice that it explicitly lists the types and values that a module
 * implementing this interface must define, as well as the exceptions that
 * any function in the interface may throw. Because of how Ocaml handles
 * exceptions, listing exceptions is optional, and you can't indicate with
 * code which functions may cause which exceptions. But it is good style to
 * mention these in comments!!!!
 *
 * Remember, functions *are* values, so functions are also listed with the
 * val keyword.
 *
 * For a function like get min, we could instead choose to return an 'a option,
 * which would avoid the need for an exception. But you should get used to
 * exceptions like these in modules, since OCaml modules tend to use them.
 *)
module type FIRSTBINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* What this type actually looks like is left up to the implementation *)
  type 'a tree

  (* Returns an empty tree *)
  val empty : 'a tree

  (* Insert elt into tree *)
  val insert : ('a -> 'a -> order) -> 'a -> 'a tree -> 'a tree

  (* Search a binary tree for the given value. See note below about
   * the first argument. *)
  val search : ('a -> 'a -> order) -> 'a -> 'a tree -> bool

  (* Delete the given value from a binary tree. See note below about
   * the first argument. May raise NodeNotFound exception. *)
  val delete : ('a -> 'a -> order) -> 'a -> 'a tree

  (* Return the minimum value of a binary tree. See note below about
   * the first argument. May raise EmptyTree exception *)
  val getmin : ('a -> 'a -> order) -> 'a tree -> 'a

  (* Return the maximum value of a binary tree. See note below about
   * the first argument. May raise EmptyTree exception *)
  val getmax : ('a -> 'a -> order) -> 'a tree -> 'a
end

(* So what's up with that first argument to search / delete / getmin / getmax?
 * Think first about an int binary search tree, where all children in the left
 * subtree are numerically smaller than the current node, and all children in
 * the right subtree are numerically higher ints than the current node. We can
 * easily use <, >, =, etc. to compare values in this tree, and determine where
 * a value should go when we are inserting it into the tree.
 *
 * But now notice that all of the values that are stored in the tree are
 * *polymorphic* (so we don't necessarily need separate modules for, say, ints
 * and floats). So we could define 'a to be ((int * string) * int list).
 * But there isn't a natural way (like with ints) to compare
 * values of this type. So, the first argument to the last four functions
 * in the signature is a function that can be used to order values
 * in the tree. So then you *can* define a "maximum" for a
 * ((int * string) * int list) tree.
 *
 * This is a clunky solution. It means that every time you want to use
 * one of these functions in the module, you have to pass in some extra
 * function indicating how the tree should be ordered.
 * What if you pass in an ordering function to delete with a tree that was
 * constructed with a different ordering function? Then delete may
 * not find the value in the tree despite it actually being there.
 *
 * The idea is that the module should keep around the ordering function from
 * the very start, instead of relying on the user to always pass it in.  How do
 * we do this? This is where functors come in. Functors allow you to define one
 * module in terms of another module. So, we can define a module for a tree by
 * using a module which defines a type and an ordering over values of that
 * type.
 *
 * For this problem set, since you haven't worked with modules yet, we will
 * include all functor-interfacing code for you.
 *)

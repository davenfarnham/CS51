(* Testing dilemma: how can I make sure that the invariant that "empty"
 * is a Leaf is satisfied?
 * The following code, if uncommented, generates the error:
 * "Error: Unbound constructor IntTree.Leaf" *)
(*
let _ =
  match IntTree.empty with
  | IntTree.Leaf _ -> ()
  | IntTree.Branch (_, _, _) -> ()
*)

(* Hmmm, so I can't see IntTree's Leaf, but maybe I can explicitly reference
 * the Leaf as defined in BinSTree? *)
(*
let _ =
  match IntTree.empty with
  | BinSTree.Leaf _ -> ()
  | BinSTree.Branch (_, _, _) -> ()
*)
(* But the above code, uncommented, likewise leads to the error:
 * "Error: Unbound constructor BinSTree.Leaf" *)

(* These errors are important and necessary.  They are an example of the way
 * in which OCaml's module system and functor system provide type abstraction.
 * We as the programmer may know that Leaf and Branch are constructors of
 * the IntTree.tree type, but in terms of OCaml's type abstraction, the
 * /client/ of the IntTree module cannot know this.
 *
 * The client of the IntTree module can only know what is provided by the
 * signature: "BINTREE with type elt = C.t" found in the definition of
 * the BinSTree functor.

 * So, for instance, we can say: *)
type element = IntTree.elt

(* And we can say: *)
let f = IntTree.delete

(* But we can't say: *)
(*
let _ = IntTree.pull_min
*)

(* Becauase the above line of code, if uncommented, would generate the
 * error: "Unbound value IntTree.pull_min" *)

(* So, the dilemma is: "how can we test the BinSTree functor, if we
 * can't look inside it"?  Well, maybe we can write tests /inside/ the BinSTree
 * functor, because inside the functor, we are inside the abstraction barrier.
 * Unfortunately, inside the functor, we don't know what the "elt" type is!
 * Specifically, we know that there is /some/ type elt, but it is abstract to us
 * -- we don't concretely know what it is.
 *
 * There is no one awesome solution to this dilemma, but we're providing you
 * with what we feel is a reasonable solution.  Look back up at the "COMPARABLE"
 * signature, and notice that it includes 4 functions related to "generate"ing
 * values of type t.  See IntCompare for an example of implementing these 4
 * functions.  Cool, right?
 *
 * Now, look back up at our testing code in BinSTree.  Notice how test_insert
 * breaks abstraction barriers and directly checks that the trees that it's
 * generating have the structure that they should.  (test_search and the others
 * /are/ written on top of the public interface of BINTREE, so they could
 * theoretically have been written anywhere, but test_insert simply must reside
 * inside the functor.)
 *
 * Finally, the last piece of the puzzle is "how do we run our tests?" Well, we
 * put run_tests in the BINTREE interface, so that it is exposed to clients of
 * IntTree and other modules that satisfy the BINTREE signature.  So, we can run
 * our tests on IntTree with the following invocation: *)
let _ = IntTree.run_tests ()


(* REQUIRED TESTING:
 * Now that you've been given a primer on testing, you are expected to do your
 * testing by filling in the run_tests function for each remaining functor that
 * you write (for each remaining functor that you write, you are expected to
 * fill in run_tests with a suite of tests whose thoroughness is on par with
 * that of run_tests in BinSTree, or better, including at least some checks
 * that "break the abstraction barrier" and truly unit test the invariants of
 * your representation, instead of just testing your public interface.
 *
 * Then, every time you invoke a functor, as in:
 *   module Foo = Functor(Argument)
 *
 * You are expected to run the tests on your new module Foo, ie:
 *   Foo.run_tests ()
 *
 * (Notice that we've also added val run_tests to the PRIOQUEUE signature,
 * so you should be able to test PRIOQUEUEs in the same way that you test
 * BINTREEs.) *)

open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;
open Expression ;;
open Helpers ;;

(*** contains_var test ***)
assert (contains_var (parse "x^4") = true);
assert (contains_var (parse "4+3") = false);
assert (contains_var (parse "4+3*x") = true);

(*** evaluate test ***)
assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);
assert (evaluate (parse "x*x + 2") 1.5 = 4.25);

(*** derivative test ***)
assert (evaluate (derivative (parse "x^3")) 2. = 12.); 
(*assert (evaluate (derivative (parse "ln(x) + x^(x*3)")) 2. = 325.584)*)

(*** find_zero test ***)
print_string ("Expected zero: 2.6684" ^ "\n" ^ "Actual: "); print_option (find_zero (parse "x^3 - 19") 3. 0.0001 5);
print_string ("Expected zero: 2.1869" ^ "\n" ^ "Actual: "); print_option (find_zero (parse "ln(x) - (x^2 - 4)") 1. 0.0001 10);
print_string ("Expected zero: .1690" ^ "\n" ^ "Actual: "); print_option (find_zero (parse "6*x-(1+x^2)^(1/2)") 0. 0.0001 5);


(*** distribute test ***)(*
print_string (to_string (distribute (parse "2 * (x + 4)"))); print_string "\n";
print_string (to_string (parse "(2 + x) * (x + 4)")); print_string "\n";
print_string (to_string (distribute (parse "(2 + x) * (x + 4)"))); print_string "\n";
print_string (to_string (distribute (parse "x * (2 + x) +  4 * (2 + x)"))); print_string "\n";
print_string (to_string (distribute (parse "5 * x - 3 + 2 * (x - 8)"))); print_string "\n";
print_string (to_string (distribute (parse "(2 * (x + 4)) * (2 * (x - 8))"))); print_string "\n";
print_string (to_string (distribute (parse "(x) / (3 * x)"))); print_string "\n";
print_string (to_string (distribute (parse "(2 * x) / (x + 4)"))); print_string "\n";
print_string (to_string (distribute (parse "(2 / x) / (x / 4)"))); print_string "\n";

(*** group test ***)
let (c, x) = (group (distribute (parse "3 * (2 + x) +  4 * (2 + x)")) 0. 0.) in
  print_float c; print_float x; print_string "\n";
print_string (to_string (distribute (parse "3 * (2 + x) +  4 * (2 + x)"))); print_string "\n";

let (c, x) = (group (distribute (parse "(2 * (4 + x)) + (2 * (8 - x))")) 0. 0.) in
  print_float c; print_float x; print_string "\n";
print_string (to_string (distribute (parse "(2 * (4 + x)) + (2 * (8 - x))"))); print_string "\n";

(*** cancel test ***)
print_string (to_string (cancel (Binop(Mul,Num 2.,Binop(Add,Var,Num 2.))) (Binop(Add,Var,Num 2.))));;
print_string "\n";

print_string (to_string (cancel (Binop(Mul,Num 2.,Binop(Mul,Var,Num 2.))) Var));;
print_string "\n";

(*** simplify test ***)
let (f,f') = simplify 5. 15. 5. in
  print_float f; print_string "\n"; print_float f'; print_string "\n";
;; *)

(*** find_zero_exact test ***)
(* -2/1 *)
let test1 = match (find_zero_exact (parse "3 * (2 + x) +  4 * (2 + x)")) with
	    | None -> print_string "None"
 	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test1
;;

(* 1/3 *)
let test2 = match (find_zero_exact (parse "3 * x - 1")) with
	    | None -> print_string "None"
 	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test2
;;

(* 19/7 *)
let test3 = match find_zero_exact ((parse "5 * x - 3 + 2 * (x - 8)")) with
	    | None -> print_string "None"
	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test3
;;

(* 8/1 *) (* shouldn't be negative
let test4 = match find_zero_exact ((parse "(5 + 2) * (x - 8)")) with
	    | None -> print_string "None"
	    | Some f -> (print_string (to_string f)); print_string "\n" in test4
;; *)

(* -2/1 *)
let test5 = match find_zero_exact ((parse "1.3 * (x + 2)")) with
	    | None -> print_string "None"
	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test5
;;

(* -2/1 *)
let test6 = match find_zero_exact ((parse "x/2 * (1 + 2) + 3")) with
	    | None -> print_string "None"
	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test6
;;

(* -18/1 *) (* give wrong answer
let test7 = match find_zero_exact ((parse "x/2 / (1 + 2) + 3")) with
	    | None -> print_string "None"
	    | Some f -> (print_string ("x = " ^ (to_string f))); print_string "\n" in test7
;; *)

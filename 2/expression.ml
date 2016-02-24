open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;
open Helpers ;;

exception Error

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Num _ -> false
  | Var -> true
  | Unop (_, e1) -> contains_var e1
  | Binop (_, e1, e2) -> contains_var e1 || contains_var e2
;;

(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Unop (u, e1) -> (match u with
		     | Sin -> (sin (evaluate e1 x)) 
     		     | Cos -> (cos (evaluate e1 x)) 
 		     | Ln -> (log (evaluate e1 x))
		     | Neg -> -.(evaluate e1 x))
  | Binop (b, e1, e2) -> (match b with
			  | Add -> (evaluate e1 x) +. (evaluate e2 x)
			  | Sub -> (evaluate e1 x) -. (evaluate e2 x)
			  | Mul -> (evaluate e1 x) *. (evaluate e2 x)
			  | Div -> (evaluate e1 x) /. (evaluate e2 x)
			  | Pow -> (evaluate e1 x) ** (evaluate e2 x))
;;

(*>* Problem 2.3 *>*)

(* See writeup for instructions. *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,(derivative e1),Unop(Cos,e1)) 
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Div,(derivative e1),e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div,Binop(Sub,Binop(Mul,(derivative e1),e2),Binop(Mul,e1,(derivative e2))),Binop(Pow,e2,Num 2.))
        | Pow ->
            if  not (contains_var e2) then Binop(Mul,e2,Binop(Mul,derivative e1,Binop(Pow,e1,Binop(Sub,e2,Num 1.))))
            else Binop(Mul,Binop(Pow,e1,e2),Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)),Binop(Div,Binop(Mul,derivative e1, e2),e1)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;

(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int) : float option =
  if lim <= 0 then None
  else if (Float.abs (evaluate e g) < epsilon) then Some g
  else find_zero (e) (g -. (evaluate e g /. (evaluate (derivative e) g))) epsilon (lim - 1)
;;

(*>* Problem 2.5 *>*)

(* Extra Credit:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
let find_zero_exact (e:expression) : expression option =
  let (c,x) = group (distribute e) 0. 0. in
    let (c',x') = simplify (Float.abs x) (Float.abs c) (Float.abs x) in
      if c < 0. then Some (Binop(Div, Num (c'), Num x'))
      else Some (Binop(Div, Num (-.c'), Num x'))
;;

(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 51;;

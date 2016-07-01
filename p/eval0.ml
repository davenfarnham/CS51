(* This file defines a substitution-based evaluator for a small
 * fragment of ML, following the lecture notes.
 *)

type variable = string ;;

type operator = Plus | Minus | Times | Div ;;

type exp = 
  | Int_e of int
  | Op_e of exp * operator * exp
  | Var_e of variable
  | Let_e of variable * exp * exp
;; 

(* Constants and Functions are values in this little
 * fragment of ML.  Operations (e.g., 3+4), variables,
 * if-then-else's, function-calls, and let's are not
 * values.  
 *)
let rec is_value (e:exp) : bool = 
  match e with 
    | Int_e _ -> true
    | (Op_e (_,_,_) | Var_e _ | Let_e (_,_,_)) -> false
;;

(****************************)
(* Some example expressions *)
(****************************)

let e1 = Op_e(Int_e 1, Plus, Int_e 41) ;; 

let e2 = Op_e(Int_e 2, Times, Int_e 21) ;;

let e3 = Let_e("x", Int_e 24, Op_e(Int_e 2, Times, Var_e "x")) ;;

let e4 = Let_e("a", Int_e 30, 
               Op_e(Let_e("a", Int_e 4, Op_e(Int_e 3, Times, Var_e "a")),
                    Plus, 
                    Var_e "a")) ;;

let test_exps = [e1; e2; e3; e4]

(* The following set of functions are used to convert expressions
 * back into strings that look like Ocaml code, so we can print 
 * them out.  I originally used the name "op2string" but 
 * switched this to "string_of_op".  Why?  *)
let string_of_op op = 
  match op with 
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" ;;

(* This function calculates the precedence of a given part of
 * the abstract syntax and is used to determine whether or not
 * we need parentheses when we print out the expression.  *)
let precedence e = 
  match e with 
    | Int_e _ -> 0
    | Var_e _ -> 0
    | Let_e (_,_,_) -> 10
    | Op_e (_,Plus,_) -> 5
    | Op_e (_,Minus,_) -> 5
    | Op_e (_,Times,_) -> 3
    | Op_e (_,Div,_) -> 3
;;

(* exp2string converts the expression e into a string, assuming
 * e occurs in a context that has precedence value prec.  If e
 * has a precedence greater than prec, then we have to put in
 * parentheses. *)
let rec exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Int_e i -> string_of_int i
      | Op_e (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var_e x -> x
      | Let_e (x,e1,e2) -> "let "^x^" = "^(exp2string 10 e1)^" in "^
          (exp2string prec e2)
  in 
    if p > prec then "(" ^ s ^ ")" else s
;;

let string_of_exp e = exp2string 10 e ;;

let e1_string = string_of_exp e1 ;;
let e2_string = string_of_exp e2 ;;
let e3_string = string_of_exp e3 ;;
let e4_string = string_of_exp e4 ;;

(* These exceptions will be used in the evaluator below to
 * signal a type-error.  Of course, for code that type-checks,
 * these errors cannot happen at run-time. *)
exception UnboundVariable of variable ;;
exception Impossible;;

(* The following function applies the operator to the two values, 
 * yielding a new value.  If the operation isn't defined on the
 * values, then we raise the BadOp exception. *)
let apply_op v1 op v2 = 
  match v1, op, v2 with 
    | Int_e i, Plus, Int_e j -> Int_e (i+j)
    | Int_e i, Minus, Int_e j -> Int_e (i-j)
    | Int_e i, Times, Int_e j -> Int_e (i*j)
    | Int_e i, Div, Int_e j -> Int_e (i/j)
    | _, _, _ -> raise Impossible 
;;

(* This function substitutes a value v for all (free) occurrences
 * of a variable within an expression e, yielding a new expression.
 * Note that we have to be careful not to substitute v for x when
 * x is shadowed by a nested function or let-expression.  For
 * example substituting v for x in "let x = 3 in x" doesn't 
 * actually change anything, because "let x = 3 in x" always 
 * evaluates to 3.  
 *)
let substitute (v:exp) (x:variable) (e:exp) : exp = 
  let rec subst (e:exp) : exp = 
    match e with 
    | Int_e _ -> e
    | Op_e (e1,op,e2) -> Op_e(subst e1,op,subst e2)
    | Var_e y -> if x = y then v else e
    | Let_e (y,e1,e2) -> 
        Let_e (y, subst e1, if x = y then e2 else subst e2)
  in 
    subst e
;;

(* Here is our main evalutor for expressions.  In general, 
 * we recursively evaluate sub-expressions to values, and
 * then operate on the values. 
 * 
 * To evaluate "let x = e1 in e2", first evaluate e1 to 
 * a value v, then substitute v for x in e2, then evaluate e2 
 * and return its value.
 * 
 * All variables should have been substituted with a value.
 * If we encounter a variable x during evaluation, then that
 * means it was never declared -- that is, there's no let-
 * expression binding it, and no function that binds it. 
 *)
let rec eval (e:exp) : exp = 
  match e with
    | Int_e i -> Int_e i 
    | Op_e (e1,op,e2) -> 
        let v1 = eval e1 in 
        let v2 = eval e2 in 
        apply_op v1 op v2
    | Let_e (x,e1,e2) -> eval (substitute (eval e1) x e2)
    | Var_e x -> raise (UnboundVariable x)
;;        

(* Now we can evaluate those expressions we built earlier *)
let eval_tests () = 
  List.iter (fun e -> Printf.printf "%s evaluates to %s\n"
                      (string_of_exp e) 
                      (string_of_exp (eval e))) test_exps
;;


(* Here is a completely equivalent piece of code -- what
 * I've done is abstracted the recursive call as the
 * function eval_loop.  Then I "tie the recursive knot"
 * in the definition of eval below.  
 *)
let eval_body (eval_loop : exp->exp) (e:exp) : exp = 
  match e with
    | Int_e i -> Int_e i 
    | Op_e (e1,op,e2) -> 
        let v1 = eval_loop e1 in 
        let v2 = eval_loop e2 in 
        apply_op v1 op v2
    | Let_e (x,e1,e2) -> eval_loop (substitute (eval_loop e1) x e2)
    | Var_e x -> raise (UnboundVariable x)
;;        
let rec eval e = eval_body eval e
;;

(* Factoring out the recursive call gives me a convenient hook
 * for writing a debugging version of the function, where I 
 * print out the expression being evaluated at the beginning
 * of each recursive call. 
 *)
let rec debug_eval e = 
  Printf.printf "Evaluating %s\n" (string_of_exp e) ; 
  let v = eval_body debug_eval e in
  Printf.printf "%s evaluated to %s\n" 
    (string_of_exp e) (string_of_exp v) ; 
  v
;;

let debug_eval_tests () = 
  List.iter (fun e -> 
               Printf.printf "Evaluating %s\n" (string_of_exp e) ;
               let v = debug_eval e in 
                 Printf.printf "Final value is %s\n" (string_of_exp v))
            test_exps
;;

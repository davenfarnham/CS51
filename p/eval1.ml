(* This file defines a substitution-based evaluator for a small
 * fragment of ML.  This file extends eval0.ml with support for
 * (non-recursive) functions, function calls, booleans, and 
 * if-then-else.  
 *)
type variable = string ;;

type constant = Int of int | Bool of bool ;;

type operator = Plus | Minus | Times | Div | LessThan | LessThanEq ;;

type exp = 
  | Constant_e of constant
  | Op_e of exp * operator * exp
  | Var_e of variable
  | If_e of exp * exp * exp
  | Fun_e of variable * exp
  | FunCall_e of exp * exp
  | Let_e of variable * exp * exp
;; 

(* Constants and Functions are values in this little
 * fragment of ML.  Operations (e.g., 3+4), variables,
 * if-then-else's, function-calls, and let's are not
 * values.  
 *)
let rec is_value (e:exp) : bool = 
  match e with 
    | Constant_e _ -> true
    | Fun_e (_,_) -> true
    | (Op_e (_,_,_) | Var_e _ | If_e (_,_,_)
      | FunCall_e (_,_) | Let_e (_,_,_)) -> false
;;

(****************************)
(* Some example expressions *)
(****************************)

(* The inc function: fun x -> x + 1 *)
let inc = Fun_e ("x", Op_e (Var_e "x", Plus, Constant_e (Int 1))) ;;

(* Declare the inc function and call it with 42: 
 * let inc = (fun x -> x + 1) in inc 42
 *)
let inc42 = Let_e ("inc", inc, FunCall_e (Var_e "inc", Constant_e (Int 42)));;

(* The minimum function:  fun x -> (fun y -> if x < y then x else y) *)
let minimum = 
  Fun_e ("x", 
         Fun_e ("y", If_e (Op_e (Var_e "x", LessThan, Var_e "y"),
                           Var_e "x",
                           Var_e "y")))
;;

(* declare the minimum function using let, and call it with 42 *)
let min42 = 
  Let_e ("min", minimum, FunCall_e (Var_e "min", Constant_e (Int 42)));;

(* apply the result of min42 to the value 0 *)
let min42zero = FunCall_e (min42, Constant_e (Int 0));;

(* The following set of functions are used to convert expressions
 * back into strings that look like Ocaml code, so we can print 
 * them out.  *)
let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b

let string_of_op op = 
  match op with 
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" 
    | LessThan -> "<" | LessThanEq -> "<=";;

(* This function calculates the precedence of a given part of
 * the abstract syntax and is used to determine whether or not
 * we need parentheses when we print out the expression.  *)
let precedence e = 
  match e with 
    | Constant_e _ -> 0
    | Var_e _ -> 0
    | Let_e (_,_,_) -> 10
    | If_e (_,_,_) -> 10
    | Fun_e (_,_) -> 10
    | FunCall_e (_,_) ->  3
    | Op_e (_,Plus,_) -> 5
    | Op_e (_,Minus,_) -> 5
    | Op_e (_,Times,_) -> 3
    | Op_e (_,Div,_) -> 3
    | Op_e (_,LessThan,_) -> 7
    | Op_e (_,LessThanEq,_) -> 7
;;

(* exp2string converts the expression e into a string, assuming
 * e occurs in a context that has precedence value prec.  If e
 * has a precedence greater than prec, then we have to put in
 * parentheses. *)
let rec exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant_e c -> string_of_const c
      | Op_e (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var_e x -> x
      | If_e (e1, e2, e3) -> 
        "if "^(exp2string 10 e1)^" then "^(exp2string 10 e2)^" else "^
          (exp2string p e3)
      | Fun_e (x,e) -> "fun "^x^" -> "^(exp2string 10 e)
      | FunCall_e (e1,e2) -> (exp2string p e1)^" "^(exp2string p e2)
      | Let_e (x,e1,e2) -> "let "^x^" = "^(exp2string 10 e1)^" in "^
          (exp2string prec e2)
  in 
    if p > prec then "(" ^ s ^ ")" else s
;;

let string_of_exp e = exp2string 10 e ;;

let inc_string = string_of_exp inc ;;
let inc42_string = string_of_exp inc42 ;;
let minimum_string = string_of_exp minimum ;;
let min42_string = string_of_exp min42 ;;
let min42zero_string = string_of_exp min42zero ;;

(* These exceptions will be used in the evaluator below to
 * signal a type-error.  Of course, for code that type-checks,
 * these errors cannot happen at run-time. *)
exception UnboundVariable of variable ;;
exception BadApplication of exp ;;
exception BadIf of exp ;;
exception BadOp of exp * operator * exp ;;

(* The following function applies the operator to the two values, 
 * yielding a new value.  If the operation isn't defined on the
 * values, then we raise the BadOp exception. *)
let apply_op v1 op v2 = 
  match v1, op, v2 with 
    | Constant_e (Int i), Plus, Constant_e (Int j) -> 
        Constant_e (Int (i+j))
    | Constant_e (Int i), Minus, Constant_e (Int j) -> 
        Constant_e (Int (i-j))
    | Constant_e (Int i), Times, Constant_e (Int j) -> 
        Constant_e (Int (i*j))
    | Constant_e (Int i), Div, Constant_e (Int j) -> 
        Constant_e (Int (i/j))
    | Constant_e (Int i), LessThan, Constant_e (Int j) -> 
        Constant_e (Bool (i<j))
    | Constant_e (Int i), LessThanEq, Constant_e (Int j) -> 
        Constant_e (Bool (i<=j))
    | _, _, _ -> raise (BadOp (v1,op,v2))
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
    | Var_e y -> if x = y then v else e
    | Constant_e _ -> e
    | Op_e (e1,op,e2) -> Op_e(subst e1,op,subst e2)
    | If_e (e1,e2,e3) -> If_e(subst e1,subst e2,subst e3)
    | FunCall_e (e1,e2) -> FunCall_e(subst e1,subst e2)
    | Fun_e (y,e1) -> if x = y then e else Fun_e (y, subst e1)
    | Let_e (y,e1,e2) -> 
        Let_e (y, subst e1, if x = y then e2 else subst e2)
  in 
    subst e
;;

(* Here is our main evalutor for expressions.  In general, 
 * we recursively evaluate sub-expressions to values, and
 * then operate on the values. 
 * 
 * Values (constants and functions) evaluate to themselves.  Note 
 * we do not evaluate the body of the function -- we wait to do that
 * until the function is called and we have the argument. 
 * 
 * To evaluate (if e1 then e2 else e3) we do not evaluate e2 and e3
 * immediately, but only e1.  Once we find out whether or not e1's 
 * value is true, we evaluate e2 or e3 respectively.  Note the 
 * possibility of a type-error in this case -- if e1 doesn't return
 * a boolean, we don't know what to do.
 * 
 * To evaluate "let x = e1 in e2", first evaluate e1 to 
 * a value v, then substitute v for x in e2, then evaluate e2 
 * and return its value.
 * 
 * Function calls "e1 e2" are very similar to let -- first we have
 * to evaluate e1 until we get a function value (fun x -> e).  
 * Then we evaluate e2 to a value v2 and substitute v2 for x
 * in e, and finally evaluate this to a value. 
 * 
 * All variables should have been substituted with a value.
 * If we encounter a variable x during evaluation, then that
 * means it was never declared -- that is, there's no let-
 * expression binding it, and no function that binds it. 
 *)
let eval_body (eval_loop : exp->exp) (e:exp) : exp = 
  match e with
    | Constant_e c -> Constant_e c 
    | Fun_e (x,e) -> Fun_e (x,e)
    | Op_e (e1,op,e2) -> 
        let v1 = eval_loop e1 in 
        let v2 = eval_loop e2 in 
          apply_op v1 op v2
    | If_e (e1,e2,e3) -> 
          (match eval_loop e1 with 
             | Constant_e (Bool true) -> eval_loop e2
             | Constant_e (Bool false) -> eval_loop e3
             | v1 -> raise (BadIf v1))
    | Let_e (x,e1,e2) -> eval_loop (substitute (eval_loop e1) x e2)
    | FunCall_e (e1,e2) -> 
        let v1 = eval_loop e1 in 
        let v2 = eval_loop e2 in 
        (match v1, v2 with 
           | Fun_e (x,e), v2 -> eval_loop (substitute v2 x e)
           | v1, _ -> raise (BadApplication v1))
    | Var_e x -> raise (UnboundVariable x)
;;        
let rec eval e = eval_body eval e
;;

(* A debugging version of the eval function, where I 
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

(* Now we can evaluate those expressions we built earlier *)
let inc_value = eval inc ;;
let inc42_value = eval inc42 ;;
let minimum_value = eval minimum ;;
let min42_value = eval min42 ;;
let min42zero_value = eval min42zero ;;

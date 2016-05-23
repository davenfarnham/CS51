(* This file extends our simple functional language evaluator with
 * support for recursive functions.   Only the changes are highlighted.
 *)

type variable = string ;;

type constant = Int of int | Bool of bool ;;

type operator = Plus | Minus | Times | Div | LessThan | LessThanEq ;;

(* The only new feature is the let-rec for defining recursive values. *)
type exp = 
  | Constant_e of constant
  | Op_e of exp * operator * exp
  | Var_e of variable
  | If_e of exp * exp * exp
  | Fun_e of variable * exp
  | FunCall_e of exp * exp
  | Let_e of variable * exp * exp
  | Letrec_e of variable * exp * exp
;; 

let rec is_value (e:exp) : bool = 
  match e with 
    | Constant_e _ -> true
    | Fun_e (_,_) -> true
    | (Op_e (_,_,_) | Var_e _ | If_e (_,_,_)
      | FunCall_e (_,_) | Let_e (_,_,_)
      | Letrec_e (_,_,_)) -> false
;;

exception UnboundVariable of variable ;;
exception BadApplication of exp ;;
exception BadIf of exp ;;
exception BadOp of exp * operator * exp ;;

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

(* Substitution is slightly different for a let-rec:  note that
 * the let-rec-bound variable shadows x for both e1 and e2.  This
 * is because when we have "let rec x = e1 in e2", x is bound in
 * both e1 and in e2.  
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
    | Letrec_e (y,e1,e2) -> 
        if x = y then Letrec_e (y,e1,e2) else Letrec_e (y,subst e1,subst e2)
  in 
    subst e
;;

let eval_body (eval_loop:exp->exp) (e:exp) : exp = 
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
        (match eval_loop e1 with 
           | Fun_e (x,e) -> eval_loop (substitute (eval_loop e2) x e)
           | v1 -> raise (BadApplication v1))
    | Var_e x -> raise (UnboundVariable x)
    (* To evaluate "let rec x = (...x...) in e2" we evaluate
     * "let x = (...(let rec x = (...x...) in x)...) in e2".
     * That is, we simply unwind the recursive definition once,
     * and then evaluate just like a let.  This works fine when
     * x is defined as a function, because we stop evaluating
     * when we hit a function.  But it doesn't work so well when
     * we don't have a function! *)
    | Letrec_e (x,e1,e2) -> 
        let e1_unwind = substitute (Letrec_e (x,e1,Var_e x)) x e1 in 
          eval_loop (Let_e (x,e1_unwind,e2))
;;        

let rec eval e = eval_body eval e
;;

let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
;;

let string_of_op op = 
  match op with 
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" 
    | LessThan -> "<" | LessThanEq -> "<=" ;;

let precedence e = 
  match e with 
    | Constant_e _ -> 0
    | Var_e _ -> 0
    | Let_e (_,_,_) -> 10
    | Letrec_e (_,_,_) -> 10
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
      | Letrec_e (x,e1,e2) -> "let rec "^x^" = "^(exp2string 10 e1)^" in "^
          (exp2string prec e2)
  in 
    if p > prec then "(" ^ s ^ ")" else s
;;

let string_of_exp e = exp2string 10 e ;;

(* fun n -> if n < 1 then 1 else n * fact (n - 1) *)
let fact_body = Fun_e ("n", 
                       If_e (Op_e (Var_e "n", LessThan, Constant_e (Int 1)),
                             Constant_e (Int 1),
                             Op_e (Var_e "n", Times, 
                             FunCall_e (Var_e "fact", 
                                        Op_e (Var_e "n", Minus, 
                                              Constant_e (Int 1))))));;

(* fact 4 *)
let fact_call = FunCall_e (Var_e "fact", (Constant_e (Int 4)));;

(* let rec fact = fun n -> if n < 1 then 1 else n * fact (n - 1) in
 * fact 4
 *)
let fact4 = Letrec_e ("fact", fact_body, fact_call) ;;

(* In this version of the debug_eval, we don't bother to 
 * print out the values. *)
let rec debug_eval e = 
  (if is_value e then () else 
     Printf.printf "Evaluating %s\n" (string_of_exp e)) ; 
  let v = eval_body debug_eval e in 
    (if is_value e then () else 
       Printf.printf "%s evaluated to %s\n" 
         (string_of_exp e) (string_of_exp v)) ; 
    v
;;

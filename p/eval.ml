(* This file extends our simple functional language evaluator with
 * support for datatypes and match-based pattern matching.  
 *)

(****************************************************************************)
(* To complete your tasks, replace calls to unimplemented with correct code *)

let unimplemented () = failwith "unimplemented";;

(****************************************************************************)

type variable = string ;;
type constructor = string ;;

type constant = Int of int ;;

type operator = Plus | Minus | Times | Div | LessThan | LessThanEq ;;

(* Data_e (d,[e1;e2;...;en]) is meant to represent a datatype expression,
 * where d is the name of the datatype constructor, and e1,e2,...,en 
 * are the arguments.  
 * 
 * For example, we represent true and false as Data_e ("true",[]) and
 * Data_e ("false",[]) respectively.  Or, we can represent options as
 * Data_e ("None", []) and Data_e ("Some", [e]).  Or we can represent
 * lists as Data_e ("Nil", []) and Data_e ("Cons", [hd; tl]). 
 *
 * Match expressions take an expression to match and a list of guards.
 * Each guard is a pattern paired with an expression.  So the match:
 *   match e with 
 *   | p1 -> e1
 *   | p2 -> e2
 *   | ...
 * is represented as Match_e(e,[(p1,e1); (p2,e2); ...]).
 *)
type exp = 
  | Constant_e of constant
  | Op_e of exp * operator * exp
  | Var_e of variable
  | Fun_e of variable * exp
  | FunCall_e of exp * exp
  | Let_e of variable * exp * exp
  | Letrec_e of variable * exp * exp
  | Data_e of constructor * (exp list)
  | Match_e of exp * ((pattern * exp) list)

(* Patterns are either constants, variables, datatype constructors
 * applied to pattern arguments, or the underscore. *)
and pattern = 
  | Constant_p of constant
  | Var_p of variable
  | Data_p of constructor * (pattern list)
  | Underscore_p
;; 

exception UnboundVariable of variable ;;
exception BadApplication of exp ;;
exception BadOp of exp * operator * exp ;;
exception BadMatch of exp ;;
exception BadPattern of pattern ;;
exception Error ;;

(* The only change here is that we use Data values to represent
 * true and false instead of built-in primitive boolean values. *)
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
        if i < j then Data_e("true",[]) else Data_e("false",[])
    | Constant_e (Int i), LessThanEq, Constant_e (Int j) -> 
        if i <= j then Data_e("true",[]) else Data_e("false",[])
    | _, _, _ -> raise (BadOp (v1,op,v2))
;;

(* Substitution for match expressions is tricky, as we must make
 * sure to not substitute for x in a guard if x occurs in the pattern 
 * of the guard.  
 *)

let rec substitute (v:exp) (x:variable) (e:exp) : exp = 
  let rec subst (e:exp) : exp = 
    match e with 
    | Var_e y -> if x = y then v else e
    | Constant_e _ -> e
    | Op_e (e1,op,e2) -> Op_e(subst e1,op,subst e2)
    | Data_e (d, es) -> Data_e (d, (List.map (fun y -> subst y) es))
    | FunCall_e (e1,e2) -> FunCall_e(subst e1,subst e2)
    | Fun_e (y,e1) -> if x = y then e else Fun_e (y, subst e1)
    | Let_e (y,e1,e2) -> 
        Let_e (y, subst e1, if x = y then e2 else subst e2)
    | Letrec_e (y,e1,e2) -> 
        if x = y then Letrec_e (y,e1,e2) else Letrec_e (y,subst e1,subst e2)
    | Match_e (e,ms) -> 
        let e' = subst e in
	  Match_e (e', List.map (fun y -> (match y with
					   | (Constant_p c, ex) -> (Constant_p c, subst ex)
					   | (Var_p x', ex) -> (Var_p x', subst (substitute v x ex)) (* might be redundant *)
					   | (Data_p (c, lst), ex) -> (match c with
								       | "Cons" -> (match lst with
										    | (Var_p hd) :: [Var_p tl] -> if x = hd || x = tl then (Data_p (c, lst), ex)
														  else (Data_p (c, lst), subst ex)
										    | (Var_p hd) :: tl -> if x = hd then (Data_p (c, lst), ex)
													  else (Data_p (c, lst), subst ex)		       
										    | hd :: [Var_p tl] -> if x = tl then (Data_p (c, lst), ex)
													  else (Data_p (c, lst), subst ex)
										    | _ -> (Data_p (c, lst), subst ex))
								       | _ -> (Data_p (c, lst), subst ex))
					   | (Underscore_p, ex) -> (Underscore_p, subst ex))) ms)
  in 
    subst e
;;

(* Evaluation an expression.
 * evaluation for matches is the tricky part -- we defined an
 * auxilliary function pattern_match for you to fill in 
 *)

(* HOW DID YOU DEAL WITH OVERLAPPING VARIABLES?   
 * (any method you choose is fine)  EXPLAIN HERE BRIEFLY:





 *)

let flatten l = 
  match l with
  | [l'] -> l'
  | _ -> raise Error
;;

let rec eval (e:exp) : exp = 
  match e with
    | Constant_e c -> Constant_e c 
    | Fun_e (x,e) -> Fun_e (x,e)
    | Op_e (e1,op,e2) -> 
        let v1 = eval e1 in 
        let v2 = eval e2 in 
          apply_op v1 op v2
    | Let_e (x,e1,e2) -> eval (substitute (eval e1) x e2)
    | FunCall_e (e1,e2) -> 
        (match eval e1 with 
           | Fun_e (x,e) -> eval (substitute (eval e2) x e)
           | v1 -> raise (BadApplication v1))
    | Var_e x -> raise (UnboundVariable x)
    | Letrec_e (x,e1,e2) -> 
        let e1_unwind = substitute (Letrec_e (x,e1,Var_e x)) x e1 in 
          eval (Let_e (x,e1_unwind,e2))
    | Data_e (d,es) -> Data_e (d,List.map eval es)
    | Match_e (e,ms) -> pattern_match (eval e) ms

(* find the first branch that matches the value v.  
 * perform pattern matching and return an expression
 * in which the appropriate parts of v have been sustituted
 * for pattern variables 
 *
 * If no patterns match the value raise the exception BadMatch
 * with expression as an argument.  ie:
 *
 * raise (BadMatch v)
 *
 *)
and pattern_match (v:exp) (ms : (pattern * exp) list) : exp = 
  match ms with
  | [] -> raise (BadMatch v)
  | (p, e') :: tl -> (match p with
		      | Constant_p c' -> if v = Constant_e c' then eval e'
					 else pattern_match v tl 
		      | Var_p v' -> eval (substitute v v' e')
		      | Data_p (c', p') -> (match v with
					    | Data_e (constr, exlst) -> if c' = constr && (p' = [] && exlst = []) then eval e'
									else (if c' = constr then pattern_match (flatten exlst) (((flatten p'), e') :: tl)
									      else pattern_match v tl)
					    | _ -> pattern_match v tl)					    
		      | Underscore_p -> eval e')
;;


(*
let increment_body : exp = 
  Fun_e ("x", 
    Match_e
      (Var_e "x",
        [(Data_p ("Nil",[]), Var_e "x");
	 (Data_p ("Cons",[Var_p "hd"; Var_p "tl"]), 
	   Data_e ("Cons", [Op_e (Var_e "hd", Plus, Constant_e (Int 1));
			   FunCall_e (Var_e "increment", Var_e "tl")]))])) ;;

            Let_e ("xs", onetwo, 
                   FunCall_e (FunCall_e (Var_e "append", Var_e "xs"), 
                              Var_e "xs"))) ;;

let eval_test (e:exp) : unit =
  Printf.printf "%s evaluates to %s\n"
    (string_of_exp e) 
    (string_of_exp (eval e))
;;
*)


(* | Data_e of constructor * (exp list) *)

(*
and pattern = 
  | Constant_p of constant
  | Var_p of variable
  | Data_p of constructor * (pattern list)
  | Underscore_p
;; 
*)

let const2string c = 
  match c with 
    | Int i -> string_of_int i
;;

let rec concats ss = 
  match ss with 
    | [] -> ""
    | s::ss -> s ^ (concats ss)
;;

let rec sep s xs = 
  match xs with 
    | [] -> xs
    | x::[] -> xs
    | x::rest -> x::s::(sep s rest)
;;

let op2string op = 
  match op with 
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" 
    | LessThan -> "<" | LessThanEq -> "<=";;

let precedence e = 
  match e with 
    | Constant_e _ -> 0
    | Var_e _ -> 0
    | Let_e (_,_,_) -> 10
    | Letrec_e (_,_,_) -> 10
    | Data_e (_,_) -> 10
    | Match_e (_,_) -> 0
    | Fun_e (_,_) -> 10
    | FunCall_e (_,_) ->  3
    | Op_e (_,Plus,_) -> 5
    | Op_e (_,Minus,_) -> 5
    | Op_e (_,Times,_) -> 3
    | Op_e (_,Div,_) -> 3
    | Op_e (_,LessThan,_) -> 7
    | Op_e (_,LessThanEq,_) -> 7
;;

let rec pat2string p = 
  match p with 
    | Constant_p c -> const2string c
    | Var_p x -> x
    | Data_p (d,[]) -> d
    | Data_p (d,ps) -> 
        d ^ " (" ^ (concats (sep "," (List.map pat2string ps))) ^ ")"
    | Underscore_p -> "_"
;;

let rec exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant_e c -> const2string c
      | Op_e (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(op2string op)^" "^(exp2string prec e2)
      | Var_e x -> x
      | Fun_e (x,e) -> "fun "^x^" -> "^(exp2string 10 e)
      | FunCall_e (e1,e2) -> (exp2string p e1)^" "^(exp2string p e2)
      | Let_e (x,e1,e2) -> "let "^x^" = "^(exp2string 10 e1)^" in "^
          (exp2string prec e2)
      | Letrec_e (x,e1,e2) -> "let rec "^x^" = "^(exp2string 10 e1)^" in "^
          (exp2string prec e2)
      | Data_e (d,[]) -> d
      | Data_e (d,es) -> 
          d ^ " (" ^ (concats (sep "," (List.map (exp2string 10) es))) ^ ")"
      | Match_e (e,ms) -> 
          "(match "^(exp2string 10 e)^" with "^
          (concats (sep " | " (List.map (fun (p,e) -> 
                                           (pat2string p) ^" -> "^
                                             (exp2string 10 e)) ms))) ^ ")"
  in 
    if p > prec then "(" ^ s ^ ")" else s
;;

let string_of_exp e = exp2string 10 e ;;

(* fun n -> match n < 1 with 0 -> 1 | n -> n * fact(n -1) *)
let fact_body = Fun_e ("n", 
                       Match_e 
                         (Op_e (Var_e "n", LessThan, Constant_e (Int 1)),
                          [ (Data_p ("true",[]), Constant_e (Int 1)) ; 
                            (Data_p ("false",[]), 
                             Op_e (Var_e "n", Times, 
                                   FunCall_e (Var_e "fact", 
                                              Op_e (Var_e "n", Minus, 
                                                    Constant_e (Int 1)))))
                          ]));;

(* fact 4 *)
let fact_call = FunCall_e (Var_e "fact", (Constant_e (Int 4)));;

(* let rec fact = fun n -> if n < 1 then 1 else n * fact (n - 1) in
 * fact 4
 *)
let fact4 = Letrec_e ("fact", fact_body, fact_call) ;;

(* fun x y -> match x with 
              | Nil -> y 
              | Cons (hd,tl) -> Cons (hd, append tl y) *)
let append_body = 
  Fun_e ("x", 
   Fun_e ("y", 
    Match_e 
      (Var_e "x", 
       [(Data_p ("Nil",[]), Var_e "y") ;
        (Data_p ("Cons",[Var_p "hd"; Var_p "tl"]), 
         Data_e ("Cons", [Var_e "hd"; 
                          FunCall_e (FunCall_e (Var_e "append", Var_e "tl"),
                                     Var_e "y")]))]))) ;;

(* Cons (1, Cons (2,Nil)) *)
let onetwo = 
  Data_e ("Cons", [Constant_e (Int 1) ; 
                   Data_e ("Cons", [Constant_e (Int 2) ; 
                                    Data_e ("Nil",[])])]) ;;

(* let rec append = fun x y -> match x with 
                     | Nil -> y | Cons (hd,tl) -> Cons (hd, append tl y) in
   let xs = Cons (1, (Cons 2, Nil)) in 
   append xs xs
*)
let appendit = 
  Letrec_e ("append", append_body, 
            Let_e ("xs", onetwo, 
                   FunCall_e (FunCall_e (Var_e "append", Var_e "xs"), 
                              Var_e "xs"))) ;;

(* TASK!!!
 *
 * replace Constant_e (Int 0) in increment_body with an exp that
 * implements a function that adds 1 to every element of a list
 *)

(* let rec increment = fun x -> match x with
			        | Nil -> x
			        | Cons(hd, tl) -> Cons(hd + 1, increment tl) in
		       let xs = Cons(1, Cons(2, Nil)) in
                       increment xs xs
*)

let increment_body : exp = 
  Fun_e ("x", 
    Match_e
      (Var_e "x",
        [(Data_p ("Nil",[]), Var_e "x");
	 (Data_p ("Cons",[Var_p "hd"; Var_p "tl"]), 
	   Data_e ("Cons", [Op_e (Var_e "hd", Plus, Constant_e (Int 1));
			   FunCall_e (Var_e "increment", Var_e "tl")]))])) ;;

let increment_all =
  Letrec_e ("increment_all", increment_body,
	    FunCall_e (Var_e "increment_all", onetwo))
;;

(* Use this for testing an expression's evaluation *)
let eval_test (e:exp) : unit =
  Printf.printf "%s evaluates to %s\n"
    (string_of_exp e) 
    (string_of_exp (eval e))
;;

let test_exps = [onetwo; fact4; appendit];;

(* Use this to evaluate multiple expressions *)
let eval_tests () = 
  List.iter eval_test test_exps
;;

(* let x = None in match x with
		   | None -> 4 + 4
		   | _ -> 4 ;; 	
*)

let match_none = Let_e("x", Data_e ("None", []),
		   Match_e (Var_e "x", 
		     [(Data_p ("None", []), Op_e (Constant_e (Int 4), Plus, Constant_e (Int 4)));
		      (Underscore_p, Constant_e (Int 4))]))

let match_some = Let_e("x", Data_e ("Some", [Constant_e (Int 4)]),
		   Match_e (Var_e "x", 
		     [(Data_p ("None", []), Op_e (Constant_e (Int 2), Plus, Constant_e (Int 2)));
		      (Data_p ("Some", [Var_p "y"]), Op_e (Var_e "y", Plus, Var_e "y"));  
		      (Underscore_p, Constant_e (Int 4))]))

let match_bool = Let_e("y", Constant_e (Int 1),
                   Let_e("x", Data_e ("true", []),
		     Match_e (Var_e "x", 
		       [(Data_p ("false", []), Op_e (Constant_e (Int 2), Plus, Constant_e (Int 2)));
		        (Data_p ("true", []), Op_e (Var_e "y", Plus, Var_e "y"))])))

let _ = 
  eval_test match_none;
  eval_test match_some;
  eval_test match_bool

(** A mini-ML
    @author Stuart M. Shieber

    This module implements a small untyped ML-like language under
    various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime generated by an explicit "raise" construct *)
exception EvalException ;;
(* more specific errors for debugging; for applications *)
exception AppException ;;
(* for binops *)
exception BinopException ;;
(* for conditionals *)
exception CondException ;;
(* for letrec in eval_s *)
exception NotImplemented
(* general purpose error *)
exception Error

module type Env_type = sig
    type env
    type value =
       | Val of expr
       | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val update : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct

    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    exception EnvUnbound

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp: expr) (env: env) : value =
      Closure (exp, env)

    (* Looks up the value of a variable in the environment *)
    let lookup (env: env) (varname: varid) : value =
      let rec loop env' = 
        match env' with
        | [] -> raise EnvUnbound
	| (var, value) :: tl -> if var = varname then !value else (loop tl) in
      loop env

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env: env) (varname: varid) (loc: value ref) : env =
      (varname, loc) :: env

    (* change a varid's value without adding a new element to the env; if updating mistakenly, extend *)
    let rec update (env: env) (varname: varid) (loc: value ref) : env =
      match env with
      | [] -> (extend env varname loc)
      | (id, v) :: tl -> if id = varname then ((id, loc) :: tl) else ((id, v) :: (update tl varname loc))

    (* Returns a printable string representation of an environment *)
    let rec env_to_string (env: env) : string =
      let rec loop env' = 
	match env' with
        | [] -> "\n"
        | (var, value) :: tl -> "var: " ^ var ^ " value: " ^ (value_to_string ~printenvp:true !value) ^ "\n" ^ (loop tl) in
      loop env

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    and value_to_string ?(printenvp : bool = true) (v: value) : string =
      match v with
      | Val e -> "Val(" ^ exp_to_string e ^ ")\n"
      | Closure (e, env) -> "Closure(" ^ if printenvp then ("exp: " ^ (exp_to_string e) ^ "env: " ^ (env_to_string env)) else (exp_to_string e) ^ ")\n"
  end
;;
	     
(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)

  
let eval_t exp _env = Env.Val exp ;;

let binop_helper (s: string) : (int -> int -> int) = 
  match s with
  | "+" -> (+)
  | "-" -> (-)
  | "*" -> ( * )
  | _ -> raise Error
;;

let unop_helper (s: string) : (int -> int) = 
  match s with
  | "~" -> (~-)
  | _ -> raise Error
;;

(* evaluate using substitution *)
let rec eval_s exp = 
  match exp with
  | Bool _ | Var _  | Num _ -> Env.Val exp 
  | Unop(s, e1) -> (match eval_s e1 with
		    | (Env.Val (Num i)) -> (Env.Val (Num ((unop_helper s) i)))
                    | _ -> raise EvalException)
  | Binop(s, e1, e2) -> (match (eval_s e1, eval_s e2) with
			 | (Env.Val (Num i), Env.Val (Num i')) -> let result = (match s with
								  	        | "=" -> Bool(( = ) i i')
									        | "<" -> Bool(( < ) i i')
									        | _ -> Num((binop_helper s) i i')) in
								  (Env.Val result)
			 | (Env.Val (Bool b), Env.Val (Bool b')) -> let result = (match s with
										  | "=" -> Bool(( = ) b b')
										  | "<" -> Bool(( < ) b b')
										  | _ -> raise BinopException) in
								    (Env.Val result)
			 | _ -> raise BinopException)
  | Conditional(e1, e2, e3) -> (match eval_s e1 with
				| (Env.Val (Bool b)) -> (match b with
							 | true -> (eval_s e2)
							 | false -> (eval_s e3))
				| _ -> raise CondException) 
  | Fun(s, e1) -> (Env.Val exp)
  | Let(s, e1, e2) -> eval_s (subst s e1 e2)
  | Letrec (s, e1, e2) -> let e1' = subst s (Letrec (s, e1, (Var s))) e1 in
			    eval_s (Let (s, e1', e2))
  | App(e1, e2) -> (match eval_s e1 with
		    | Env.Val (Fun (x, p)) -> eval_s (Let(x, e2, p))
		    | _ -> raise AppException)
  | Raise | Unassigned -> (Env.Val exp)
;;

(* evaluate dynamically using environment; don't deal with closures yet *)
let rec eval_d exp env_ =  
  match exp with
  | Bool _ | Num _ -> (Env.Val exp)
  | Var x -> (Env.lookup env_ x)
  | Unop(s, e1) -> (match eval_d e1 env_ with
		    | (Env.Val (Num i)) -> (Env.Val (Num ((unop_helper s) i)))
                    | _ -> raise EvalException)
  | Binop(s, e1, e2) -> (match (eval_d e1 env_, eval_d e2 env_) with
			 | (Env.Val (Num i), Env.Val (Num i')) -> let result = (match s with
								  	        | "=" -> Bool(( = ) i i')
									        | "<" -> Bool(( < ) i i')
									        | _ -> Num((binop_helper s) i i')) in
								  (Env.Val result)
			 | (Env.Val (Bool b), Env.Val (Bool b')) -> let result = (match s with
										  | "=" -> Bool(( = ) b b')
										  | "<" -> Bool(( < ) b b')
										  | _ -> raise BinopException) in
								    (Env.Val result)
			 | _ -> raise BinopException)
  | Conditional(e1, e2, e3) -> (match eval_d e1 env_ with
				| Env.Val (Bool b) -> (match b with
						       | true -> eval_d e2 env_
						       | false -> eval_d e3 env_)
			        | _ -> raise CondException)
  | Fun(s, e1) -> (Env.Val exp)
  | Let(s, e1, e2) -> (match eval_d e1 env_ with
		       | Env.Val v -> let env_' = Env.extend env_ s (ref (Env.Val v)) in
					eval_d e2 env_'
		       | Env.Closure _ -> raise Error)
  | Letrec(s, e1, e2) -> let env_' = Env.extend env_ s (ref (Env.Val Unassigned)) in
			   (match eval_d e1 env_' with
			    | Env.Val Unassigned -> raise EvalException
			    | Env.Val v -> let env_'' = Env.update env_' s (ref (Env.Val v)) in
					     eval_d e2 env_''  
			    | Env.Closure _ -> raise Error)
  | App(e1, e2) -> (match eval_d e1 env_ with
		    | Env.Val (Fun (s, e1')) -> (match eval_d e2 env_ with
			                         | Env.Val v -> let env_' = Env.extend env_ s (ref (Env.Val v)) in
						                  eval_d e1' env_'
				                 | Env.Closure _ -> raise Error) 	
		    | _ -> raise AppException)
  | Raise | Unassigned -> (Env.Val exp)
;;

(* lexically scoped environment *)
let rec eval_l exp env_ = 
  match exp with
  | Bool _ | Num _ -> (Env.Val exp)
  | Var x -> (Env.lookup env_ x) 
  | Unop(s, e1) -> (match eval_l e1 env_ with
		    | (Env.Val (Num i)) -> (Env.Val (Num ((unop_helper s) i)))
                    | _ -> raise EvalException)
  | Binop(s, e1, e2) -> (match (eval_l e1 env_, eval_l e2 env_) with
			 | (Env.Val (Num i), Env.Val (Num i')) -> let result = (match s with
								  	        | "=" -> Bool(( = ) i i')
									        | "<" -> Bool(( < ) i i')
									        | _ -> Num((binop_helper s) i i')) in
								  (Env.Val result)
			 | (Env.Val (Bool b), Env.Val (Bool b')) -> let result = (match s with
										  | "=" -> Bool(( = ) b b')
										  | "<" -> Bool(( < ) b b')
										  | _ -> raise BinopException) in
								    (Env.Val result)
			 | _ -> raise BinopException)
  | Conditional(e1, e2, e3) -> (match eval_l e1 env_ with
				| Env.Val (Bool b) -> (match b with
						       | true -> eval_l e2 env_
						       | false -> eval_l e3 env_)
			        | _ -> raise CondException)

  (* for every function, keep track of its env *)
  | Fun(s, e1) -> (Env.Closure (exp, env_))

  | Let(s, e1, e2) -> (match eval_l e1 env_ with
		       (* add value to environment *)
		       | Env.Val v -> let env_' = Env.extend env_ s (ref (Env.Val v)) in
					eval_l e2 env_'
		       | Env.Closure (v, viron) as temp -> let env_' = Env.extend env_ s (ref temp) in
						             eval_l e2 env_')

  | Letrec(s, e1, e2) -> let env_' = Env.extend env_ s (ref (Env.Val Unassigned)) in
			   (match eval_l e1 env_' with
			    | Env.Val Unassigned -> raise EvalException
			    | Env.Val v -> let env_'' = Env.update env_' s (ref (Env.Val v)) in
					     eval_l e2 env_''  
			    | Env.Closure (v, viron) -> let viron' = Env.update viron s (ref (Env.Val v)) in
							  let env_'' = Env.update env_' s (ref (Env.Closure (v, viron'))) in
							    eval_l e2 env_'')							  

  | App(e1, e2) -> (match eval_l e1 env_ with
		    | Env.Closure (Fun (s, e1'), env_') -> (match eval_l e2 env_ with
			                         	    | Env.Val v as t -> let env_'' = Env.extend env_' s (ref t) in
								                  eval_l e1' env_''
							    | Env.Closure c as t -> let env_'' = Env.extend env_' s (ref t) in
								       	              eval_l e1' env_'')
		    | Env.Val (Fun (s, e1')) -> (match eval_l e2 env_ with
			                    	 | Env.Val v as t -> let env_' = Env.extend env_ s (ref t) in
						                       eval_l e1' env_'
						 | Env.Closure v as t -> let env_' = Env.extend env_ s (ref t) in
						             		   eval_l e1' env_')
		    | _ -> raise AppException)

  | Raise | Unassigned -> (Env.Val exp)
 
let evaluate = eval_t ;;

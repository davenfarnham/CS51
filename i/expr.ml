(** Abstract syntax of MiniML expressions **)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | List of (expr list)
  | Concat of expr * expr
 and varid = string ;;

exception ExpToStringError
exception Error
  
(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;
  
type varidset = SS.t ;;

(* counter to create new vars *)
let counter = ref 0 ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(** Return a set of the variable names free in [exp] *)
let free_vars (exp : expr) : varidset =
  let empty_vars = SS.empty in
    let rec loop set exp' =  
      (match exp' with
       | Num _ | Raise | Unassigned | Bool _ -> set
       | Var x -> (SS.add x set)
       | Unop (v, e) -> (loop set e) 
       | Binop (v, e1, e2) -> (SS.union (loop set e1) (loop set e2))
       | Conditional (e1, e2, e3) -> (SS.union (SS.union (loop set e1) (loop set e2)) (loop set e3))
       | Fun (v, e) -> (SS.remove v (loop set e)) 
       | Let (v, e1, e2) -> (loop set (App (Fun (v, e2), e1)))
       | Letrec (v, e1, e2) -> (SS.remove v (loop set (App (Fun (v, e2), e1))))
       | App (e1, e2) -> (SS.union (loop set e1) (loop set e2))
       | List e1 -> (match e1 with
		     | [] -> set 
		     | hd :: tl -> (SS.union (loop set hd) (loop set (List tl))))
       | Concat (e1, e2) -> (SS.union (loop set e1) (loop set e2))) in
    loop empty_vars exp
;;  

(* couple quick tests *)
let _ = 
  let e = (Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3)))) in
    let s = free_vars e in 
      assert (SS.elements s = [])
;;

let _ = 
  let e = Let ("x", Num(3), Let ("y", Var("x"), App (App (Var("f"), Var("x")), Var("y")))) in
    let s = free_vars e in
      assert (SS.elements s = ["f"])
;;

let _ =
  let e =  Let ("x", Num(4), Binop ("+", Var("x"), Var("y"))) in
    let s = free_vars e in
      assert (SS.elements s = ["y"])
;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname () : varid =
  (counter := !counter + 1); ("var" ^ (string_of_int (!counter)))
;;
  
(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  let rec subst' (var_name': varid) (repl': expr) (exp': expr) : expr = 
    match exp' with
    | Num _ | Bool _ | Raise | Unassigned -> exp'
    | Var x -> if x = var_name' then repl' else exp'
    | Unop (s, e) -> Unop(s, (subst' var_name' repl' e))
    | Binop (s, e1, e2) -> Binop(s, (subst' var_name' repl' e1), (subst' var_name' repl' e2))
    | Conditional (e1, e2, e3) -> Conditional((subst' var_name' repl' e1), (subst' var_name' repl' e2), (subst' var_name' repl' e3))
    | Fun (s, e1) -> (match (s = var_name') with
                      | true -> Fun(s, e1) 
		      | false -> let fv = free_vars repl in
				   (match (SS.mem s fv) with
				    | true -> let z = new_varname() in
					        Fun(z, (subst' var_name' repl' (subst' s (Var z) e1)))
				    | false -> Fun(s, (subst' var_name' repl' e1))))
    | Let(s, e1, e2) -> (match s = var_name' with
			 | true -> Let(s, (subst' var_name' repl' e1), e2)
			 | false -> let fv = free_vars repl in 
				      (match SS.mem s fv with
				       | true -> let z = new_varname() in
						   Let(z, (subst' var_name' repl' e1), (subst' var_name' repl' (subst' s (Var z) e2)))
				       | false -> Let(s, (subst' var_name' repl' e1), (subst' var_name' repl' e2))))
    | App(e1, e2) -> App(subst' var_name' repl' e1, subst' var_name' repl' e2) 
    | Letrec (s, e1, e2) ->(match s = var_name' with
                            | true -> exp
                            | false -> let fv = free_vars repl in
                                         (match SS.mem s fv with
                                          | true -> let z = new_varname() in
                                                      Letrec(z, (subst' var_name' repl' e1), (subst' var_name' repl' (subst' s (Var z) e2)))
                                          | false -> Letrec(s, (subst' var_name' repl' e1), (subst' var_name' repl' e2)))) 
    | _ -> raise Error in
  subst' var_name repl exp
;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  match exp with
  | Var x -> x
  | Num i -> (string_of_int i)
  | Bool b -> (match b with
	       | true -> "true"
	       | false -> "false")
  | Unop (s, e) -> "Unop(" ^ s ^ ", " ^ (exp_to_string e) ^ ")"
  | Binop (s, e1, e2) -> (exp_to_string e1) ^ " " ^ s ^ " " ^ (exp_to_string e2)
  | Conditional (e1, e2, e3) -> "Conditional (" ^ (exp_to_string e1) ^ ", " ^(exp_to_string e2) ^ ", " ^ (exp_to_string e3) ^ ")" 
  | Fun (s, e1) -> "Fun (" ^ s ^ ", " ^ (exp_to_string e1) ^ ")"
  | Let (s, e1, e2) -> "Let (" ^ s ^ ", " ^ (exp_to_string e1) ^ ", " ^ (exp_to_string e2) ^ ")"
  | Letrec (s, e1, e2) -> "Letrec (" ^ s ^ ", " ^ (exp_to_string e1) ^ ", " ^ (exp_to_string e2) ^ ")"
  | App (e1, e2) -> "App (" ^ (exp_to_string e1) ^ ", " ^ (exp_to_string e2) ^ ")"
  | Raise -> "Raise"          
  | Unassigned -> "Unassigned"                         
  | List e -> let pl l = 
		let front = "[" in
		  let rec pl' l' = 
		    (match l' with
	       	     | [] -> "]"
		     | hd :: [] -> (exp_to_string hd) ^ "]"
                     | hd :: tl -> (exp_to_string hd) ^ ";" ^ (pl' tl)) in
                  (front ^ pl' l) in
	      (pl e)  
  | Concat (e1, e2) -> (exp_to_string e1) ^ "::" ^ "[" ^ (exp_to_string e2) ^ "]"
;;

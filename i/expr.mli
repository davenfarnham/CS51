
(** Abstract syntax of MiniML expressions *)

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
 and varid = string

type varidset ;;

(* Test to see if two sets have the same elements (for
   testing purposes) *)
val same_vars: varidset -> varidset -> bool;;
(* Generate a set of variable names from a list of strings (for
   testing purposes) *)
val vars_of_list: varid list -> varidset ;;
(* Returns the set of varids corresponding to free variables in
       the expression *)
val free_vars: expr -> varidset
(* Returns a freshly minted varid *)
val new_varname: unit -> varid
(* subst x p q returns the expression q with p substituted for
       free occurrences of x *)
val subst: varid -> expr -> expr -> expr
(* exp_to_string e returns a string represnetation of the abstract
       syntax of the expression e *)
val exp_to_string: expr -> string
    

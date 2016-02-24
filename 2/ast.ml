(* ------ Type definitions for the abstract syntax tree ------- *)

(* Binary operators. *)
type binop = Add | Sub | Mul | Div | Pow

(* Unary operators. *)
type unop = Sin | Cos | Ln | Neg

type expression =
  | Num of float
  | Var
  | Binop of binop * expression * expression
  | Unop of unop * expression

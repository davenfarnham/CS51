open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;

exception Error

(* 
 * Most functions here are used for the extra credit: find_zeros_exact.      
 * My solution isn't complete; most notably it doesn't handle division well, 
 * both in canceling terms and adding terms with common denominators. A      
 * number of bugs should be addressed, 1) "x/2 / (1 + 2) + 3" isn't          
 * evaluated correctly, and 2) "(5 + 2) * (x - 8)" comes back negative       
 * when it should be positive. Future additions should handle division       
 * better and cancel variables in the denominator, since if you can't	       
 * cancel an "x" in the denominator, there can't be a root.		       
 *)

let rec contains_var (e:expression) : bool =
  match e with
  | Num _ -> false
  | Var -> true
  | Unop (_, e1) -> contains_var e1
  | Binop (_, e1, e2) -> contains_var e1 || contains_var e2
;;

(* print value out of an option *)
let print_option (f : float option) : unit = 
  match f with
  | None -> ()
  | Some r -> print_float r; print_string "\n"
;;

(* create a summation by distributing all products and quotients *)
let rec distribute (e:expression) : expression = 
  match e with
  | Num _ -> e
  | Var -> e
  | Binop(b,l,r) -> 
	(match b with
	 | Add -> Binop(Add,distribute l,distribute r)
	 | Sub -> Binop(Sub,distribute l,distribute r)
	 | Mul -> 
	      (match l with
	       | Binop(Mul,_,_) -> 
		 (match r with 
		  | Binop(Mul,l',r') -> distribute (Binop(Mul,Binop(Mul,distribute l,distribute l'),distribute r')) 
		  | Binop(Div,l',r') -> distribute (Binop(Div,Binop(Mul,distribute l,distribute l'),distribute r'))
		  | Binop(Add,l',r') -> distribute (Binop(Add,Binop(Mul,distribute l,distribute l'), 
			 			  	      Binop(Mul,distribute l,distribute r')))
		  | Binop(Sub,l',r') -> distribute (Binop(Sub,Binop(Mul,distribute l,distribute l'),
				        	  	      Binop(Mul,distribute l,distribute r')))
		  | _ -> Binop(Mul,distribute l,r)) 
	       | Binop(Div,_,_) -> 
		 (match r with 
		  | Binop(Mul,l',r') -> distribute (Binop(Mul,Binop(Div,distribute l,distribute l'),distribute r'))
		  | Binop(Div,l',r') -> distribute (Binop(Mul,Binop(Div,distribute l,distribute l'),distribute r'))
  	     	  | Binop(Add,l',r') -> distribute (Binop(Add,Binop(Div,distribute l,distribute l'), 
						  	      Binop(Div,distribute l,distribute r')))
		  | Binop(Sub,l',r') -> distribute (Binop(Sub,Binop(Div,distribute l,distribute l'),
				         	  	      Binop(Div,distribute l,distribute r')))
		  | _ -> Binop(Mul,distribute l,r))
	       | Binop(op,_,_) ->
		 (match r with
		  | Binop(_,l',r') -> distribute (Binop(op,Binop(Mul,distribute l,distribute l'),Binop(Mul,distribute l,distribute r')))
		  | _ -> distribute (Binop(Mul,r,l)))
	       | _ -> 
		 (match r with
		  | Binop(Mul,l',r') -> distribute (Binop(Mul,Binop(Mul,l,distribute l'),distribute r'))
		  | Binop(Div,l',r') -> distribute (Binop(Div,Binop(Mul,l,distribute l'),distribute r'))
		  | Binop(Add,l',r') -> distribute (Binop(Add,Binop(Mul,l,distribute l'),Binop(Mul,l,distribute r')))
		  | Binop(Sub,l',r') -> distribute (Binop(Sub,Binop(Mul,l,distribute l'),Binop(Mul,l,distribute r')))
		  | _ -> e))
	 | Div -> 	
	      (match l with
	       | Binop(Mul,l',r') ->
		 (match r with 
		  | Binop(Mul,l',r') -> distribute (Binop(Div,Binop(Div,distribute l,distribute l'),distribute r'))
		  | Binop(Div,l',r') -> distribute (Binop(Div,Binop(Div,distribute l,distribute l'),distribute r'))
		  | Binop(Add,_,_) -> distribute (Binop(Mul,Binop(Div,distribute l',distribute r),r'))
		  | Binop(Sub,l',r') -> distribute (Binop(Sub,Binop(Div,distribute l,distribute l'),
				        	  	     Binop(Div,distribute l,distribute r')))
		  | _ -> Binop(Div,distribute l,r)) 
	       | Binop(Div,l',r') ->
		 (match r with
		  | Binop(Mul,_,_) -> distribute (Binop(Div,Binop(Mul,distribute l',distribute r),distribute r'))
		  | Binop(Div,lr,rr) -> distribute (Binop(Div,Binop(Mul,distribute l',distribute rr),
							      Binop(Mul,distribute r',distribute lr)))
		  | Binop(Add,_,_) -> distribute (Binop(Div,Binop(Mul,distribute l',distribute r),r')) 
		  | Binop(Sub,_,_) -> distribute (Binop(Div,Binop(Mul,distribute l',distribute r),r'))
		  | _ -> Binop(Div,distribute l,r)) 
	       | Binop(op,_,_) ->
		 (match r with
		  | Binop(_,l',r') -> distribute (Binop(op,Binop(Div,distribute l,distribute l'),Binop(Div,distribute l,distribute r')))
		  | _ -> distribute (Binop(Mul,r,l)))
	       | _ -> 
		 (match r with
		  | Binop(Mul,l',r') -> distribute (Binop(Div,Binop(Div,l,distribute r'),distribute l'))
		  | Binop(Div,l',r') -> distribute (Binop(Div,Binop(Mul,l,distribute r'),distribute l'))
		  | Binop(Add,_,_) | Binop(Sub,_,_) -> (Binop(Div,l,r))
		  | _ -> e))
	 | _ ->e)
  | Unop (_,_) -> e
;;

(* cancel if (simplified exp in denominator with Var) = (exp in numerator with Var) *)
let rec cancel (numerator:expression) (denominator:expression) : (expression) = 
  if numerator = denominator then Num 1.
  else (match numerator with
	| Binop(b,l,r) -> Binop(b, cancel l denominator, cancel r denominator) 
	| _ -> numerator)	
;;	

(* group variables and constants together *)
let rec group (e:expression) (c:float) (x:float) : (float * float) =
  match e with
  | Num f -> ((c +. f),x)
  | Var -> (c,x)
  | Binop(Add,l,r) -> let (cons, vars) = group l c x in
		        let (cons', vars') = group r c x in
			  (cons +. cons', vars +. vars') 
  | Binop(Sub,l,r) -> let (cons, vars) = group l c x in
		        let (cons', vars') = group r c x in
			  (cons -. cons', vars -. vars') 
  | Binop (b,l,r) -> 
	if contains_var e then (match l with
				| Var -> (if contains_var r then raise Error
					  else let (c',_) = group r c x in
					         if x = 0. then (c, c')
						 else if b = Mul then (c, x *.c')
						 else (c, x /. c'))
			        | Num f -> if x = 0. then group r c f
					   else if b = Mul then group r c (x *. f)
					   else group r c (x /. f)
			        | Binop(_,_,_) -> (match r with
				 		   | Num f -> if x = 0. then group l c f
							      else group l c (x *. f)
				 		   | Var -> (if contains_var l then raise Error 
							    else group l c x)
					           | Binop(Div,_,_) -> raise Error
					           | Binop(_,_,_) -> raise Error
						   | _ -> raise Error) (* todo *)
			        | _ -> raise Error) (* todo *)
	else (match l with
	      | Num f -> (match r with
			  | Num f' -> if c = 0. then ((f *. f', x))
				      else ((c *. f *. f'), x)
			  | Var -> raise Error
			  | Binop(_,_,_) -> group r (c *. f) x
			  | _ -> raise Error) (* todo *)
	      | Var -> raise Error
	      | Binop(_,_,_) -> (match r with
				 | Num f -> group l (c *. f) x
				 | Var -> raise Error
				 | Binop(_,_,_) -> raise Error (* todo *)
			         | _ -> raise Error) (* todo *)
	      | _ -> raise Error) (* todo *)
  | _ -> raise Error (* todo *)
;;

(* round to nearest integer value, but keep it a float *)
let float_round (f:float) : float =
  let floor = Float.round_down (f) in 
    if (floor +. 0.5 <= f) then (floor +. 1.)
    else floor
;; 

(* mod 2 floats *)
let float_mod (f:float) (f':float) : float = 
  let fl = float_round (f *. 100.) in
    let fr = float_round (f' *. 100.) in 
      let q = (Float.to_int fl) / (Float.to_int fr) in 
        ((fl -. ((Float.of_int q) *. fr)) /. 100.)
;;

(* basic simlpifiation *)
let rec simplify (i:float) (n:float) (d:float) : (float * float) = 
  if i <= 1. || (Float.abs n) = 1. || (Float.abs d) = 1. then (n,d)
  else if ((Float.abs (float_mod n i)) < 0.01) && ((Float.abs (float_mod d i)) < 0.01) then simplify (d /. i -. 1.) (n /. i) (d /. i)
  else simplify (i -. 1.) n d
;;

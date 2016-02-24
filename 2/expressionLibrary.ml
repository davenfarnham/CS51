(* ------------- Library code ------------- *)
(* You shouldn't have to do anything here.  *)

open Core.Std ;;
open Ast ;;

exception ParseError of string

type token =
  | NumT of float
  | VarT
  | BinopT of binop
  | UnopT of unop
  | LParen
  | RParen
  | LBrace
  | RBrace
  | EOF

let recognized_tokens = [|"x"; "sin"; "cos"; "ln"|] ;;

let token_expressions = [|VarT; UnopT Sin; UnopT Cos; UnopT Ln|] ;;

let string_to_char_list (s:string) : char list =
  let rec string_to_char_list' (s:string) (acc:char list) (i:int) =
    if i < 0 then acc else
      let c = String.get s i in
    string_to_char_list' s (c::acc) (i-1)
  in string_to_char_list' s [] (String.length s - 1)
;;

let is_digit (c:char) : bool =
  let i = int_of_char c in
    i >= 48 && i <= 57
;;

(* The precedence of a binary operator.  Used in the parse_string and
      to_string_smart functions. *)
let binop_precedence (b:binop) : int =
  match b with
    | Add -> 3
    | Sub -> 3
    | Mul -> 2
    | Div -> 2
    | Pow -> 1
;;

let unop_precedence (_:unop) : int = 4 ;;

(* A strict upper bound on the precedence of any operator. *)
let prec_bound : int = 5 ;;

let binop_is_associative (b:binop) : bool =
  match b with
    | Add | Mul -> true
    | Sub | Div | Pow -> false ;;

(* Pretty-printing functions for expressions *)

let binop_to_string (b:binop) : string =
  match b with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Pow -> "^"
;;

let unop_to_string (u:unop) : string =
  match u with
    | Sin -> "sin"
    | Cos -> "cos"
    | Ln -> "ln"
    | Neg -> "~"
;;

let token_to_string (t:token) : string =
  match t with
    | NumT n -> Float.to_string n
    | VarT -> "x"
    | BinopT b -> binop_to_string b
    | UnopT u -> unop_to_string u
    | LParen -> "("
    | RParen -> ")"
    | LBrace -> "{"
    | RBrace -> "}"
    | EOF -> "EOF"
;;

(* Only adds parentheses when needed to prevent ambiguity. *)
let to_string_smart (e:expression) : string =
  let rec to_string_smart' e parent_precedence parent_associative =
    match e with
      | Num n ->
      if n >= 0.0 then Float.to_string n
      else "~" ^ Float.to_string (Float.abs n)
      | Var -> "x"
      | Unop (u,e1) ->
      unop_to_string u ^ "(" ^
        to_string_smart' e1 (unop_precedence u) false ^ ")"
      | Binop (b,e1,e2) ->
      let prec = binop_precedence b in
          let e_str =
          (to_string_smart' e1 prec false ^
           binop_to_string b ^
           to_string_smart' e2 prec (binop_is_associative b)) in
            if prec > parent_precedence ||
                  (prec = parent_precedence && not parent_associative)
            then "(" ^ e_str ^ ")"
        else e_str
  in to_string_smart' e prec_bound false
;;

(* Always adds parentheses around all binary ops. Completely unambiguous;
       however, often very hard to read... *)
let rec to_string (e:expression) : string =
  match e with
    | Num n ->
    if n >= 0.0 then Float.to_string n
        else "~" ^ Float.to_string (Float.abs n)
    | Var -> "x"
    | Unop (u,e1) -> "(" ^ unop_to_string u ^ "(" ^ to_string e1 ^ "))"
    | Binop (b,e1,e2) ->
        "(" ^ to_string e1 ^ binop_to_string b ^ to_string e2 ^ ")"
;;

(* Lexing functions (producing tokens from char lists) *)

let rec match_while (p:char -> bool) (l:char list) : string * char list =
  match l with
    | [] -> ("", [])
    | c::cs ->
    if p c then
      let (s_cs, l_cs) = match_while p cs in (String.make 1 c ^ s_cs, l_cs)
    else ("", l) ;;

let lex_number_string = match_while (fun c -> is_digit c || c = '.')

let lex_number (l:char list) : (token * char list) option =
  let (s,l') = lex_number_string l in
    try Some (NumT (Float.of_string s), l')
    with Invalid_argument _ -> None ;;

let rec match_string (l:char list) (s:string) : char list option =
  if s = "" then Some l else
    match l with
      | [] -> None
      | h::t ->
      if h = String.get s 0 then
            match_string t (String.sub s ~pos:1 ~len:(String.length s - 1))
          else None ;;

let lex_multi_char_token (l:char list) : (token * char list) option  =
  let rec lex_multi_char_token' l i =
    if i >= Array.length recognized_tokens then None
    else
      match match_string l (Array.get recognized_tokens i) with
    | Some l' -> Some (Array.get token_expressions i, l')
    | None -> lex_multi_char_token' l (i+1)
  in lex_multi_char_token' l 0 ;;

let rec lex' (l:char list) : token list =
  match l with
    | [] -> []
    | ' '::cs -> lex' cs
    | c::cs ->
    let (token, l') =
      (match c with
       | '+' -> (BinopT Add, cs)
       | '-' -> (BinopT Sub, cs)
       | '*' -> (BinopT Mul, cs)
       | '/' -> (BinopT Div, cs)
       | '^' -> (BinopT Pow, cs)
       | '~' -> (UnopT Neg, cs)
       | '(' -> (LParen, cs)
       | ')' -> (RParen, cs)
       | '{' -> (LBrace, cs)
       | '}' -> (RBrace, cs)
       | _ ->
           (match lex_number l with
        | Some (t, l') -> (t, l')
        | None ->
            (match lex_multi_char_token l with
             | Some (t, l') -> (t, l')
             | None -> raise (ParseError "Unrecognized token"))))
    in token :: lex' l' ;;

let lex (s:string) : token list =
  lex' (string_to_char_list s) @ [EOF] ;;

let parse (s:string) : expression =
  let rec parse_toplevel_expression (l:token list) : expression =
    let (e,_,_) = parse_delimited_expression l EOF prec_bound in e

  and parse_expression (l:token list) : expression * token list =
    match l with
      | [] -> raise (ParseError "Unexpected end of string")
      | t::ts ->
          match t with
            | LParen ->
        let (e,l',_) =
          parse_delimited_expression ts RParen prec_bound in
          (e,l')
            | RParen -> raise (ParseError "Unexpected rparen")
            | LBrace ->
        let (e,l',_) =
          parse_delimited_expression ts RBrace prec_bound in
          (e,l')
            | RBrace -> raise (ParseError "Unexpected rbrace")
            | UnopT u -> parse_unop ts u
            | VarT -> (Var, ts)
            | EOF -> raise (ParseError "Unexpected EOF")
            | NumT n -> (Num n, ts)
            | BinopT _ ->
        raise (ParseError ("Unexpected Binop: " ^ token_to_string t))

  and parse_binop (l:token list) (delim:token) (current_prec:int) eq
      : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 2")
      | t::ts ->
          if t = delim then (eq,ts,true) else
            match t with
              | BinopT b ->
                  let prec = binop_precedence b in
                    if current_prec <= prec then (eq,l,false)
                    else
              let (eq2,l',d) =
                        parse_delimited_expression ts delim prec in
            if d then (Binop(b,eq,eq2),l',true)
            else parse_binop l' delim current_prec
                          (Binop(b,eq,eq2))
              | _ ->
          raise
            (ParseError
                       ("Expecting Binop, but found: " ^ token_to_string t))

  and parse_delimited_expression (l:token list) (delim:token)
      (current_prec:int) : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 3")
      | t::_ ->
          if t = delim then
            raise (ParseError ("Unexpected delim: " ^ token_to_string delim))
          else
        let (eq,l') = parse_expression l in
              parse_binop l' delim current_prec eq

  and parse_unop (tokens:token list) (u:unop) =
    let (e,t) = parse_expression tokens in (Unop(u,e),t)

  in parse_toplevel_expression (lex s)
;;

let () = Random.self_init ()
let rec make_exp l =
  match l with
    | 0 -> (match (Random.int 2) with
          | 0 -> Num ((Float.of_int (Random.int 100)) -. 50.)
          | _ -> Var )
    | _ -> (match (Random.int 8) with
          | 0 -> Num ((Float.of_int (Random.int 100)) -. 50.)
          | 1 -> Var
          | 2 | 3 | 4 | 5 -> (let binexp = match (Random.int 5) with
            | 0 -> Add
            | 1 -> Sub
            | 2 -> Mul
            | 3 -> Div
            | _ -> Pow in
             Binop(binexp,(make_exp (Random.int (l - 1))),
               (make_exp (Random.int (l - 1)))))
          | _ -> (let unexp = match (Random.int 4) with
            | 0 -> Sin
            | 1 -> Cos
            | 2 -> Neg
            | _ -> Ln in
             Unop(unexp, (make_exp (Random.int (l - 1))))));;

let rand_exp_str l = to_string_smart (make_exp l);;

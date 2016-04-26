/* MiniML: a subset of ML
   Parser
   Stuart M. Shieber
   November 18, 2015
 */
		  
%{
  open Printf ;;
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS 
%token TIMES
%token LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token TRUE FALSE

%left PLUS MINUS
%left TIMES
%nonassoc LESSTHAN
%nonassoc EQUALS

%start input
%type <Expr.expr> input

/* Grammar follows */
%%
input:	exp EOF			{ $1 }

exp: 	exp expnoapp   		{ App($1, $2) }
	| expnoapp		{ $1 }

expnoapp: INT			{ Num $1 }
	| TRUE			{ Bool true }
	| FALSE			{ Bool false }
	| ID			{ Var $1 }
	| exp PLUS exp		{ Binop("+", $1, $3) }
	| exp MINUS exp		{ Binop("-", $1, $3) }
	| exp TIMES exp		{ Binop("*", $1, $3) }
	| exp EQUALS exp	{ Binop("=", $1, $3) }
	| exp LESSTHAN exp	{ Binop("<", $1, $3) }
	| NEG exp		{ Unop("~", $2) }
	| IF exp THEN exp ELSE exp
	     	      	        { Conditional($2, $4, $6) }
	| LET ID EQUALS exp IN exp	{ Let($2, $4, $6) }
	| LET REC ID EQUALS exp IN exp	{ Letrec($3, $5, $7) }
	| FUNCTION ID DOT exp	{ Fun($2, $4) }	
	| RAISE			{ Raise }
	| OPEN exp CLOSE	{ $2 }
;

%%

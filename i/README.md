# Interpreter (CS51 Spring 2016)

A basic interpreter that goes through the substitution method, dynamic scoping, and lexical scoping.
To change which it uses, alter the "res = " call in miniml.ml. 

## Additions

I added basic functionality for lists. There's no type checking, but you can concat and work with lists,
for example:

	-> let x = 5 in [x + 1; x + 2; x + 3] ;;
	-> [6;7;8]

	-> 5 :: [6;7;8]
	-> [5;6;7;8]

	-> 5 :: 6
	-> Error


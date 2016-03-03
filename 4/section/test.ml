open Core.Std

    (* define signature for comparing *)
    module type COMPARING =
        sig    
            (* type of things I'm comparing *)
            type t 
            (* actual comparison functions *)
            val compare : t -> t-> int
        end

    (* define a module implementing the above signature for ints but NOT sealed by it*)
    module IntCompare  =
        struct
            type t = int
            
            let compare t1 t2 = if t1 > t2 then 1 else if t2 > 1 then -1 else 0;;       
        end

    (* define a module implementing the above signature for strings AND sealed by it *)
    module StringCompare : COMPARING  = 
        struct
            
            type t = string
            
            let compare t1 t2 = String.compare t1 t2;;
        
        end

    (* I'm not going to be able to look into the StringCompare module. Typing "StringCompare.compare;;" in utop will yield abstraction. I can
       still look in the IntCompare module, however, since I didn't seal it. *) 


    (* Now let's have a functor that does a lot of things, including comparing things: *)
   
    (* SET is a sig for a module that takes a module that matches the signature COMPARING, and will return a module *)
    module type SET = 
      functor (Comparing : COMPARING) -> 
        sig
            (* set the element in this signature equal to type 't' in comparing *)
            type e = Comparing.t (* set 'e' equal to 't'. Since we are setting e equal to something here, if Comparing.t is visible
                                    e  will also be visible. *)
            (* this type, however, won't be visible *)
	    type set
            (* empty list won't depend on e *)
            val emp : set
	    (* add an element to the set *)
	    val add : e -> set -> set
            (* take an element from the set *)
	    val take : set -> (e * set) option
            (* size*)
            val size : set -> int
            (* I can't compare ints and strings in the same way *)
            val compare : e -> e -> int                        
        end

    (* Module that matches signature SET, using module Comparing that implements signature COMPARING *)
    module Set : SET = 
        functor (Comparing : COMPARING) -> 
            struct
                open Comparing
                
                type e = t (* If I pass in StringCompare, then e = StringCompare.t; if I pass in IntCompare, e = int *)
                
                type set = (e list) (* the list, since it is sealed in the SET signature, is abstracted away *)
                
                let emp = [];;

		let add e s = e :: s;;

		let take s = match s with
		             | [] -> None
			     | hd :: tl -> Some(hd, tl);;	       
				     
                let size = (List.length);;
                
                let compare t1 t2 = Comparing.compare t1 t2;;            
            end




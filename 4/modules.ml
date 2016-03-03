(*****************************************************************************)
(*                           Part 1 - Warmup                                 *)
(*****************************************************************************)

open Core.Std

(*>* Problem 1.0 *>*)
module type MATH =
sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
end

(* Write a module called Math that implements the MATH signature above 
 * We have started the module for you below*)

(* 
module Math: ??? =
struct
   ???
end
*)

(*>* Problem 1.1 *>*)

(*
 * Write a signature called LIST that only exposes the functions length,
 * fold_right, and rev. The Core.Std.List documentation might be helpful:
 * https://ocaml.janestreet.com/ocaml-core/109.60.00/doc/core/#Std.List
 * We have given you the beginnings of the signature below. 
*)

(*
module type ??? =
sig
  ???
end
*)

(* The following should work after you have created your signature (you
 * can uncomment them *)


(*
module MyList = (List : LIST);;

let _ =
    assert(MyList.length [1;2;3] = 3);
    assert(MyList.fold_right ~f:(+) ~init:0 [1;2;3] = 6);
    assert(MyList.rev [1;2;3] = [3;2;1])
*)


(* Even with your signature, the following line should never compile:

MyList.fold_left ~f:(+) ~init:0 [1;2;3]

*)


(*>* Problem 1.2 *>*)
module Allison =
struct
    type house =
        Adams | Lowell | Quincy |
        Kirkland | Winthrop | Eliot |
        Leverett | Mather | Dunster |
        Pforzheimer | Cabot | Currier

    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : house
    }

    let hometown = "Riverside, CA"
    let year = 2015
    let concentration = "Computer Science"
    let house = Adams
    let fold = List.fold_left ~f:(+)

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let grade_assignment assignment =
      "Everyone gets a perfect score for pset " ^ String.strip assignment ^ "!"

    let favorite_function _ = failwith "I don't have a favorite function"
    let least_favorite_function = ( ** )

    let print_info () =
        let _ = print_string (
            info.hometown ^ "\n" ^
            string_of_int year ^ "\n" ^
            info.concentration ^ "\n") in
        match info.house with
        | Adams -> print_string "Adams!\n"
        | _ -> failwith "Do any other houses matter?"

end

module Ben =
struct
    type info = {
        hometown : string;
        house : string;
        year : int;
        concentration : string
    }

    let least_favorite_function = (land)

    let rec info = {
        hometown = hometown;
        house = house;
        year = 2015;
        concentration = "Computer Science"
    }
    and hometown = "Holland, Michigan"
    and house = "Dunster"

    let grade_assignment assignment =
      "Everyone gets a zero for pset " ^ string_of_int assignment ^ ">:-("

    let favorite_function x y =
        log x +. log y +. Int64.to_float 0x46524F535459L

    let print_info = fun _ ->
        print_string (info.house ^ "\n" ^ info.hometown ^ "\n" ^
            string_of_int info.year ^ "\n" ^ info.concentration)

    let f = (-)
    let fold = List.fold_right ~f

end

(* Write a signature TF that exposes as much of *both* Ben
 * and Allison as possible. *)

(* The follow should work after you define the TF signature *)
(*
module TFBen = (Ben : TF)
module TFAllison = (Allison : TF)
*)

(*>* Problem 1.3 *>*)

(* Challenge (worth 1 point)!

 * Write another module that matches sig TF. Try to fill
 * in the info of your own TF! If you don't have a TF, or
 * your TF is already listed here, then feel free to
 * create a module for Henry, Jesse, or yourself.
 *
 * You should not trivially reuse any of the same values as
 * were used above in order to implement the module.
 *)

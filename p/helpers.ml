(* Additional polymorphic helper functions *)

exception HeadError
exception TailError

(* take element out of list *)
let head l =
  match l with
  | hd :: _ -> hd
  | _ -> raise HeadError
;;

let tail l =
  match l with
  | _ :: tl -> tl
  | _ -> raise TailError
;;

(* inherent distrust of the List signatures *)
let rec fold_right f acc l =
  match l with
  | [] -> acc
  | hd :: tl -> fold_right f (f hd acc) tl
;;

let rec fold_left f u l =
  match l with
  | [] -> u
  | hd :: tl -> f hd (fold_left f u tl)
;;



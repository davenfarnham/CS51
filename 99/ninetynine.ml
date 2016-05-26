open Core.Std

(* #1 *)
let rec last l = 
  match l with
  | [] -> None
  | hd :: tl -> (match last tl with
		 | None -> Some hd
		 | Some hd' -> Some hd')
;;

assert(last [ "a" ; "b" ; "c" ; "d" ] = Some "d")
assert(last [] = None)


(* #2 *)
let rec last_two l = 
  match l with
  | [] -> None
  | _ :: [] -> None
  | hd :: tl :: [] -> Some (hd, tl)
  | _ :: tl :: rest -> last_two (tl :: rest)
;;

assert(last_two [ "a" ] = None)
assert(last_two [] = None)
assert(last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"))


(* #3 *)
let at n l = 
  let rec loop count l' = 
    match l' with
    | [] -> None
    | hd :: tl -> if n = count then Some hd else (loop (count + 1) tl) in
  loop 1 l
;;

assert(at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c")
assert(at 3 [ "a" ] = None)


let rec fold_right f l acc = 
  match l with
  | [] -> acc
  | hd :: tl -> fold_right f tl (f hd acc)
;;

(* #4 *)
let length l = 
  fold_right (fun _ y -> 1 + y) l 0
;;

assert(length [1;2;3;4;5;6] = 6)
assert(length [ "a" ; "b" ; "c"] = 3)
assert(length [] = 0)


(* #5 *)
let rec rev l = 
  match l with
  | [] -> []
  | hd :: tl -> (rev tl) @ [hd]
;;

assert(rev [1;2;3;4;5;6] = [6;5;4;3;2;1])
assert(rev [ "a" ; "b" ; "c"] = ["c"; "b"; "a"])
assert(rev [] = [])


(* #6 *)
let is_palindrome l = 
  let r = rev l in 
    let rec loop l' r' = 
      (match l', r' with
       | [], [] -> true
       | [], _ :: _ | _ :: _, [] -> false 
       | hd :: tl, hd' :: tl' -> if hd = hd' then loop tl tl'
				else false) in
    loop l r
;;

assert(is_palindrome [] = true)
assert(is_palindrome [ "a" ; "b" ] = false)
assert(is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true)


(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
   nodes. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

(* #7 *)
let rec flatten l = 
  match l with
  | [] -> []
  | (One a) :: tl -> [a] @ (flatten tl)
  | Many l :: tl -> (flatten l) @ (flatten tl)
;;

assert(flatten [One "a"; Many[One "b"; Many[One "c";One "d"]; One "e"]] = 
       ["a"; "b"; "c"; "d"; "e"])


(* #8 *)
let rec compress l = 
  match l with
  | [] -> []
  | hd :: [] -> [hd]
  | hd :: tl :: rest -> if hd = tl then compress (tl :: rest)
			else hd :: (compress (tl :: rest))
;;

assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
		["a"; "b"; "c"; "a"; "d"; "e"])


(* #9 *)
let pack l =
  let rec sub_list l' acc = 
    (match l' with
     | [] -> acc, None
     | hd :: [] -> hd :: acc, None
     | hd :: tl :: rest -> if hd = tl then (sub_list (tl :: rest) (hd :: acc))
			   else (hd :: acc), Some (tl :: rest)) in
    let rec loop l'' = 
      (match sub_list l'' [] with
       | hd, None -> [hd]
       | hd, (Some tl) -> hd :: (loop tl)) in 
    loop l 
;;

assert(pack [] = [[]])
assert(pack ["a";"a";"b"] = [["a";"a"];["b"]])
assert(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
	    [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; 
	     ["e"; "e"; "e"; "e"]])


let rec map f l = 
  match l with 
  | [] -> []
  | hd :: tl -> f hd :: (map f tl)
;;

exception Error

(* #10 *)
let encode l =
  map (fun x -> (match x with
                 | [] -> raise Error
                 | hd :: _ -> (length x, hd))) (pack l)
;;

assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
	      [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])

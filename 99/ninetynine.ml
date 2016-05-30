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


(* street fighter interview question *)
type keystroke = Up | Down | Left | Right | A | B
type tree = Leaf | Empty | Branch of (string * tree * tree * tree * tree * tree * tree) 

(* empty tree refs *)
let t = ref (Branch("", Leaf, Leaf, Leaf, Leaf, Leaf, Leaf)) 
let full = ref Empty

let check t = 
  match t with 
  | Leaf -> (Branch("", Leaf, Leaf, Leaf, Leaf, Leaf, Leaf))
  | _ -> t

(* essentially a trie implemented using a tree *)
let register (l : keystroke list) (name : string) : unit = 
    let rec insert l' t' = 
      (match t' with
       | Branch(s, u, d, l, r, a, b) -> (match l' with
      					 | [] -> Branch(name, u, d, l, r, a, b)
					 | Up :: tl -> Branch(s, insert tl (check u), d, l, r, a, b)
					 | Down :: tl -> Branch(s, u, insert tl (check d), l, r, a, b)
					 | Left :: tl -> Branch(s, u, d, insert tl (check l), r, a, b)
					 | Right :: tl -> Branch(s, u, d, l, insert tl (check r), a, b)
					 | A :: tl -> Branch(s, u, d, l, r, insert tl (check a), b)
					 | B :: tl -> Branch(s, u, d, l, r, a, insert tl (check b))) 
       | _ -> raise Error) in
  (t := (insert l !t))
;;

let is_name tr = 
  match tr with
  | Branch (s, _, _, _, _, _, _) -> if not (s = "") then (print_string (s ^ "\n"); (t := !full); true) else (t := tr; false)
  | _ -> ((t := !full); false)

let on_key (k: keystroke) : bool = 
  match !t with
  | Leaf | Empty -> false
  | Branch(_, u, d, l, r, a, b) -> (match k with
				    | Up -> is_name u
				    | Down -> is_name d
				    | Left -> is_name l
				    | Right -> is_name r
				    | A -> is_name a
				    | B -> is_name b)

let play l = 
  (full := !t);
  let rec loop l' = 
    (match !t with
     | Branch(_, Leaf, Leaf, Leaf, Leaf, Leaf, Leaf) -> ((t := !full); loop l')
     | _ -> (match l' with
             | [] -> ()
      	     | hd :: tl -> ignore (on_key hd); loop tl)) in
  loop l


let _ = 
    (register [Up; Down; Up; Down] "Punch"); 
    (register [Left; Right; Left; Up] "Kick");
    (register [Up; Up; Down; Down; Left; Right; Left; Right; B; A] "Konami");
    play [Up; Down; Up; Down; Left; Right; Left; Up]; 								(* Punch; Kick *)
    play [Up; Down; Up; Down; A; Left; Right; A; Left; Up];							(* Punch; Kick is interrupted *)
    play [Up; Down; Up; Down; Up; Up; Down; Down; Left; Right; Left; Right; B; A; Left; Right; Left; Up]	(* Punch; Konami; Kick *)


type 'a rle = | One of 'a | Many of int * 'a

(* 11 *)
let rec encode' l = 
  match l with
  | [] -> []
  | hd :: [] -> [One hd]
  | hd :: tl -> (match encode' tl with
		 | [] -> []
 		 | (One a) :: tl' -> if hd = a then (Many (2, a)) :: tl' else (One hd) :: (One a) :: tl'
		 | (Many (i, a)) :: tl' -> if hd = a then (Many (i + 1, a)) :: tl' else (One hd) :: (Many (i, a)) :: tl')

assert(encode' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = 
	       [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])


(* 12 *)
let rec decode l = 
  match l with
  | [] -> []
  | hd :: tl -> (match hd with
		 | One a -> a :: (decode tl)
		 | Many (i, a) -> if i = 2 then a :: decode ((One a) :: tl) 
				  else (a :: decode ((Many (i - 1, a)) :: tl)))		  

assert(decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = 
	      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])


(* 13 - already do this in 11 *)

let rec map f l = 
  match l with
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl)

let rec fold_left f l i =
  match l with
  | [] -> i
  | hd :: tl -> f hd (fold_left f tl i) 

(* 14 *)
let duplicate l =
  fold_left (fun x y -> x :: x :: y) l []

assert(duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])


(* 15 *)
let replicate l n = 
  let rec loop x n' = 
    if n' = 0 then [] else x :: (loop x (n' - 1)) in
  fold_left (fun x y -> (loop x n) @ y) l []

assert(replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])


(* 16 *)
let drop l n = 
  let rec loop l' n' =   
    match l' with
    | [] -> []
    | hd :: tl -> if n' = n then loop tl 1 else hd :: (loop tl (n' + 1)) in
  loop l 1

assert(drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])


(* 17 *)
let split l n =
  let rec loop l' n' = 
    match l' with
    | [] -> ([], [])
    | hd :: tl -> if n' = n then ([hd], tl) else (match (loop tl (n' + 1)) with
						  | (l, r) -> (hd :: l, r)) in
  loop l 1

assert(split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]))
assert(split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], []))


(* 18 *)
let slice l i i' = 
  let rec loop l' left right = 
    match l' with
    | [] -> []
    | hd :: tl -> if ((left = i) || (left > i)) && ((right = i') || (right < i')) then hd :: (loop tl (left + 1) (right + 1))
		  else (loop tl (left + 1) (right + 1)) in
  loop l 0 0 

assert(slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"])


(* 19 *)
let rec rotate l n = 
  if n = 0 then l else (match l with
			| [] -> []
			| hd :: tl -> if n > 0 then rotate (tl @ [hd]) (n - 1) else (rotate l ((List.length l) + n)))

assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])
assert(rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) =["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])


let head l = 
  match l with
  | [] -> raise Error
  | hd :: _ -> hd

let tail l = 
  match l with
  | [] -> []
  | _ :: tl -> tl

(* 20 *)
let remove_at n l = 
 let rec loop n' l' = 
   if n = n' then (tail l') else ((head l') :: (loop (n' + 1) (tail l'))) in
  loop 0 l

assert(remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"])


(* 21 *)
let rec insert_at s p l = 
  match l with
  | [] -> [s]
  | hd :: tl -> if p = 0 then s :: l else hd :: (insert_at s (p - 1) tl)

assert(insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"])
assert(insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"])
assert(insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"])


(* 22 *)
let rec range b t = 
  if b < t then (if b = t then [b] else (b :: range (b + 1) t))
  else (if b = t then [b] else (b :: range (b - 1) t))

assert(range 4 9 = [4; 5; 6; 7; 8; 9])
assert(range 9 4 = [9; 8; 7; 6; 5; 4])


(* 23 - solution online reverses list for some reason *)
open Random 
let rec rand_select l n =
  if n = 0 then [] else let pos = Random.int (List.length l) in 
    			  let r = slice l pos pos in
      			    (rand_select (List.rev (slice l 0 (pos - 1)) @ (slice l (pos + 1) ((List.length l) - 1))) (n - 1)) @ r

assert(rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3 = ["g"; "d"; "a"])


(* 24 *)
let lotto_select n m = 
  rand_select (range 1 m) n

assert(lotto_select 6 49 = [10; 20; 44; 22; 41; 2])


(* 25 *)
let permutation l = 
  rand_select l (List.length l)

assert(permutation ["a"; "b"; "c"; "d"; "e"; "f"] = ["a"; "e"; "f"; "b"; "d"; "c"])


(* 26 *)
let extract n l = 
  let rec inner hd tl = 
    match tl with
    | [] -> []
    | _ :: tl' -> if (List.length tl' < (n - 1)) then [(hd :: slice tl 0 (n - 2))] else ((inner hd tl') @ [(hd :: slice tl 0 (n - 2))]) in
  let rec outer l'' =
    let dis = (List.length l'') - n in
      if dis < 0 then [] else ((outer (tail l'')) @ (inner (head l'') (tail l''))) in
  outer l

assert(extract 2 ["a";"b";"c";"d"] = [["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]])
assert(extract 3 ["a";"b";"c";"d"] = [["b";"c";"d"];["a";"c";"d"];["a";"b";"c"]])
assert(extract 3 ["a";"b"] = [])


(* 27 - without using previous functions *)
let group lst p = 
  match p with
  | [l;r] -> 
  | _ -> raise Error

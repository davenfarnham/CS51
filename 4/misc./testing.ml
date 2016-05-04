open Huffman

exception Error

(* test insert *)
let _ = assert (insert (5, ["a"]) Empty = Branch(Even, (5, ["a"]), Leaf, Leaf))
let _ = assert (insert (6, ["b"]) (Branch(Even, (5, ["a"]), Leaf, Leaf)) = 
		Branch(Odd, (5, ["a"]), Branch(Even, (6, ["b"]), Leaf, Leaf), Leaf))
let _ = assert (insert (5, ["f"]) (insert (9, ["e"]) 
		                (insert (16, ["d"]) (insert (12, ["c"]) 
					          (insert (13, ["b"]) (insert (45, ["a"]) Empty))))) = 
		Branch (Odd, (5, ["f"]), (Branch (Even, (9, ["e"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Branch (Even, (16, ["d"]), Leaf, Leaf))), 
					 (Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf))))


(* test get_last *)
let _ = assert (get_last (insert (6, ["b"]) (Branch(Even, (5, ["a"]), Leaf, Leaf))) = Some ((6, ["b"]), Branch(Even, (5, ["a"]), Leaf, Leaf)))
let _ = 
  let t = (Branch (Odd, (5, ["f"]), (Branch (Even, (9, ["e"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Branch (Even, (16, ["d"]), Leaf, Leaf))), 
					 	  (Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf)))) in
    match get_last t with
    | None -> assert(false)
    | Some (v, t) -> assert (v = (16, ["d"]));
    		     assert (t = Branch (Even, (5, ["f"]), (Branch (Odd, (9, ["e"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf)),
                                          		   (Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf))))


(* test fix *)
let _ = 
  let t = fix (Branch (Even, (16, ["d"]), (Branch (Odd, (9, ["e"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf)), 
                                          (Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf)))) in
	  assert (t = Branch (Even, (9, ["e"]), (Branch (Odd, (16, ["d"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf)),
				                (Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf))))
let _ = 
  let t = fix (Branch (Odd, (13, ["b"]), Branch (Odd, (16, ["d"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf),
				         Branch (Even, (12, ["c"]), Leaf, Leaf))) in
  	  assert (t = Branch (Odd, (12, ["c"]), Branch (Odd, (16, ["d"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf),
                                         Branch (Even, (13, ["b"]), Leaf, Leaf)))


(* test take *)
let _ = assert (take (insert (5, ["f"]) (insert (9, ["e"]) 
		                (insert (16, ["d"]) (insert (12, ["c"]) 
					          (insert (13, ["b"]) (insert (45, ["a"]) Empty)))))) = 
		Some ((5, ["f"]), Branch (Even, (9, ["e"]), (Branch (Odd, (16, ["d"]), Branch (Even, (45, ["a"]), Leaf, Leaf), Leaf)), 
					 		(Branch (Odd, (12, ["c"]), Branch (Even, (13, ["b"]), Leaf, Leaf), Leaf)))))						



(* test search *)
let _ = 
  let rec fold_left l f i = 
    match l with
    | [] -> i
    | hd :: tl -> (f hd (fold_left tl f i)) in

  let fs = [(45, ["a"]); (13, ["b"]); ( 12, ["c"]); (16, ["d"]); (9, ["e"]); (5, ["f"])] in
    let (_, decoding) = encode fs in
      let s = search decoding [0;1;0;1;1;0;0;1;1;1;1;1;0;1;1;1;0;0] in
        let s' = fold_left s (fun x y -> x ^ y) "" in
          assert (s' = "abcdef")


(* print out encoding *)
let _ =
  let fs = [(45, ["a"]); (13, ["b"]); ( 12, ["c"]); (16, ["d"]); (9, ["e"]); (5, ["f"])] in
    let (encoding, decoding) = encode fs in
      Encoding.iter (fun x y -> print_string (x ^ ":" ^ y ^ "\n")) encoding; (breadth decoding)

open Util ;;    
open CrawlerServices ;;
open Order ;;
open Nodescore ;;
open Graph ;;


(* Dictionaries mapping links to their ranks. Higher is better. *)
module RankDict = Dict.Make(
  struct 
    type key = link
    type value = float
    let compare = link_compare
    let string_of_key = string_of_link
    let string_of_value = string_of_float
    let gen_key () = {host=""; port=0; path=""}
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = 0.0
    let gen_pair () = (gen_key(),gen_value())
  end)

module PageSet = Myset.Make(
  struct 
    type t = page
    let compare = (fun a b -> link_compare (a.url) (b.url))
    let string_of_t = string_of_page
    let gen () = {url={host=""; port=0; path=""}; links=[]; words=[]}
    let gen_lt x () = gen ()
    let gen_gt y () = gen ()
    let gen_random () = gen ()
    let gen_between x y () = None
  end)

module LinkSet = Myset.Make(
  struct 
    type t = link
    let compare = link_compare
    let string_of_t = string_of_link
    let gen () = {host=""; port=0; path=""}
    let gen_lt x () = gen ()
    let gen_gt y () = gen ()
    let gen_random () = gen ()
    let gen_between x y () = None
  end)

module PageGraph = Graph (
  struct
    type node = link
    let compare = link_compare
    let string_of_node = string_of_link
    let gen () = {host=""; port=0; path=""}
  end)

module PageScore = NodeScore (
  struct
    type node = link
    let string_of_node = string_of_link
    let compare = link_compare
    let gen () = {host=""; port=0; path=""}
  end)

(* Given a bunch of pages, convert them to a graph *)
let graph_of_pages (pages : PageSet.set) : PageGraph.graph =
  (* Only want graph nodes for pages we actually crawled *)
  let crawled_links = 
    PageSet.fold (fun page s -> LinkSet.insert page.url s)
      LinkSet.empty pages 
  in
  let add_links page graph =
    let add_link g dst =
      if LinkSet.member crawled_links dst then
        PageGraph.add_edge g page.url dst
      else g 
    in
      List.fold_left add_link graph page.links
  in
    PageSet.fold add_links PageGraph.empty pages
      
(* The rest of the world wants a RankDict, not a NodeScore. *)

let dict_of_ns (ns : PageScore.node_score_map) : RankDict.dict =
  PageScore.fold (fun node score r -> RankDict.insert r node score) 
    RankDict.empty ns

(* A type for modules that can compute nodescores from graphs *)
module type RANKER = 
sig
  module G: GRAPH
  module NS: NODE_SCORE
  val rank : G.graph -> NS.node_score_map
end


(* Each node's rank is equal to the number of pages that link to it. *)
module InDegreeRanker  (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) : 
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA
  let rank (g : G.graph) = 
    let add_node_edges ns node =
      let neighbors = match G.neighbors g node with
        | None -> []
        | Some xs -> xs
      in
        List.fold_left (fun ns' neighbor -> NS.add_score ns' neighbor 1.0) 
          ns neighbors 
    in
    let nodes = (G.nodes g) in
      List.fold_left add_node_edges (NS.zero_node_score_map nodes) nodes
end



(*****************************************************************)
(* Random Walk Ranker                                            *)
(*****************************************************************)
module type WALK_PARAMS =
sig
  (* Should we randomly jump somewhere else occasionally? 
    if no, this should be None.  Else it should be the probability of 
    jumping on each step *)
  val do_random_jumps : float option
  (* How far must sisyphus walk? *)
  val num_steps : int
end

module RandomWalkRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) 
  (P : WALK_PARAMS) : 
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA

  exception Error

  let rec index_into_list (index: int) (lst : 'a list) : 'a = 
    match lst with
    | [] -> raise Error
    | hd :: tl -> if index = 0 then hd else (index_into_list (index - 1) tl)

  let rec print_node_list lst =
    match lst with
    | [] -> ()
    | hd :: tl -> print_string (GA.N.string_of_node hd); print_node_list tl

  let prob (pr : float option) : bool = 
    match pr with
    | None -> false
    | Some p -> let p' = Random.float 1.0 in 
      		  if (p' >= p) then true else false

  let rank (g : G.graph) =
    let nodes = (G.nodes g) in
      let random_start = Random.int (List.length nodes) in
      let rec loop (count : int) (nsm : NS.node_score_map) node =
        if (prob P.do_random_jumps) then loop count nsm (index_into_list (Random.int (List.length nodes)) nodes)
	else (if count = 0 then nsm 
	      else let neighboring_list = (match G.neighbors g node with
	             | None -> []
	             | Some xs -> xs) in
	        let nsm' = NS.add_score nsm node 0.1 in 
	          let length' = List.length neighboring_list in
	            let node' = (if length' = 0 then (index_into_list (Random.int (List.length nodes)) nodes)
			         else index_into_list (Random.int (List.length neighboring_list)) neighboring_list) in
	              loop (count - 1) nsm' node') in
      NS.normalize (loop P.num_steps (NS.zero_node_score_map nodes) (index_into_list random_start nodes))
end

(*****************************************************************)
(* KARMA                                                         *)
(* Quantum Ranker                                                *)
(*****************************************************************)


module type QUANTUM_PARAMS =
sig
  (* What fraction of each node's score should be uniformly distributed
     to the graph as a whole? *)
  val alpha : float
  (* How many rounds? *)
  val num_steps : int
    
  (* Print stuff? *)
  val debug : bool
end

module QuantumRanker (GA: GRAPH) (NSA: NODE_SCORE with module N = GA.N) 
  (P : QUANTUM_PARAMS) : 
  (RANKER with module G = GA with module NS = NSA) =
struct
  module G = GA
  module NS = NSA

  exception Error
		
  let rec update_list lst nsm v = 
    match lst with
    | [] -> nsm
    | hd :: tl -> update_list tl (NS.add_score nsm hd v) v

  let deop_score s =
    match s with
    | None -> raise Error
    | Some f -> f 

  let rank (g : G.graph) = 
    let nodes = G.nodes g in 
      let num_nodes = List.length nodes in 
        let nsm = update_list nodes (NS.zero_node_score_map nodes) 1.0 in
          let rec loop count nlst nsm = 
      	    match nlst with
	    | [] -> if count = 0 then nsm else (loop (count - 1) nodes nsm)
	    | hd :: tl -> let neighboring_list = (match G.neighbors g hd with
						| None -> []
						| Some xs -> xs) in 
  			    let nsm' = update_list neighboring_list nsm ((deop_score (NS.get_score nsm hd)) /. (float_of_int (List.length neighboring_list))) in
			      let nsm'' = update_list nodes nsm' ((P.alpha *. (deop_score (NS.get_score nsm' hd))) /. (float_of_int num_nodes)) in
	    loop count tl nsm'' in
	NS.normalize(loop (P.num_steps - 1) nodes nsm)		
end



(*******************  TESTS BELOW  *******************)

module TestInDegreeRanker =
struct 
  module G = NamedGraph
  let g = G.add_edge G.empty "a" "b";;
  let g2 = G.add_edge g "a" "c";;
  
  module NS = NodeScore (struct
                           type node = string 
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = InDegreeRanker (G) (NS);;
  let ns = Ranker.rank g2;;
  (* let _ = Printf.printf "NS: %s\n" (NS.string_of_node_score_map ns) ;; *)
  assert ((NS.get_score ns "a") = Some 0.0);;
  assert ((NS.get_score ns "b") = Some 1.0);;
  assert ((NS.get_score ns "c") = Some 1.0);;
  assert ((NS.get_score ns "d") = None);;

  let g3 = G.add_edge g2 "b" "c";;
  let ns2 = Ranker.rank g3;;
  assert ((NS.get_score ns2 "a") = Some 0.0);;
  assert ((NS.get_score ns2 "b") = Some 1.0);;
  assert ((NS.get_score ns2 "c") = Some 2.0);;

end


module TestRandomWalkRanker =
struct 
  module G = NamedGraph
  let g = G.from_edges [("a","b") ; 
                        ("b","c") ;
                        ("c","d") ;
                        ("d","e") ;
                        ("e","f") ;
                        ("a","g") ;
                        ("g","a")]
  
  module NS = NodeScore (struct
                           type node = string 
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = RandomWalkRanker (G) (NS) 
    (struct 
       let do_random_jumps = None
       let num_steps = 1000
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing RandomWalkRanker:\n NS: %s\n" 
    (NS.string_of_node_score_map ns) 

(* That's the problem with randomness -- hard to test *)
end


module TestQuantumRanker =
struct 
  module G = NamedGraph
  let g = G.from_edges [("a","b") ; 
                        ("a","c") ;
                        ("b","c") ;
                        ("c","a")]
  
  module NS = NodeScore (struct
                           type node = string 
                           let compare = string_compare
                           let string_of_node = fun x -> x
                           let gen () = ""
                         end);;

  module Ranker = QuantumRanker (G) (NS) 
    (struct 
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)

  let ns = Ranker.rank g
  let _ = Printf.printf "Testing Quantum ranker:\n %s\n" 
    (NS.string_of_node_score_map ns) 

(* That's the problem with randomness -- hard to test *)
end


open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;

exception NoPage

(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  (*  
     = InDegreeRanker (PageGraph) (PageScore)
  *)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
   = QuantumRanker (PageGraph) (PageScore) (struct 
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set) (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  if n = 0 then d 
  else (match LinkSet.choose frontier with
  	| None -> d
  	| Some (l, s) -> 
	      if LinkSet.member visited l then crawl n s visited d 
	      else 
	          match CrawlerServices.get_page l with
	          | None -> crawl n s visited d (* continue along frontier if page doesn't exist *)
	          | Some p -> let rec add_to_dict l d' = match l with
					         	 | [] -> d' 
					                 | hd :: tl -> match WordDict.lookup d' hd with
							               | None -> add_to_dict tl (WordDict.insert d' hd (LinkSet.singleton p.url))
							               | Some w -> add_to_dict tl (WordDict.insert d' hd (LinkSet.insert p.url w)) in
			      let rec add_to_set l s' = match l with
						        | [] -> s'
							| hd :: tl -> add_to_set tl (LinkSet.insert hd s') in
			      crawl (n-1) (add_to_set p.links frontier) (LinkSet.insert p.url visited) (add_to_dict p.words d))
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)

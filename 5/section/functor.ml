open Core.Std

(* a simplified, shortened verson of SET *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool
  
  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option
end

module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
end
        
module IntCompare  =
struct
  type t = int
            
  let compare t1 t2 = if t1 > t2 then Greater else if t2 > 1 then Less else Equal;;       
end

module Queue =
struct
  type 'a t = 'a list

  let emp : 'a t = []

  let put (q : 'a t) (v : 'a) : 'a t =
    q @ [v]
  ;;

  let take (q : 'a t) : ('a * 'a t) option =
    match q with
    | [] -> None
    | hd :: tl -> (Some (hd, tl))
  ;;
end

module QueueSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct

  type elt = C.t
  type set = C.t Queue.t
    
  let empty = Queue.emp
    
  let is_empty d = (d = empty)
                             
  let insert e s = Queue.put s e

  let singleton e = Queue.put Queue.emp e

  let rec union s1 s2 = match (Queue.take s1) with
                        | None -> s2
                        | Some (e, s') -> union s' (Queue.put s2 e) 
                        
                         (* (Queue.put (union s' s2) e) *)

  let rec member s e = match Queue.take s with
                       | None -> false
                       | Some (e', s') -> (match C.compare e e' with
                                          | Equal -> true
                                          | _ -> member s' e);;
                                          
  let choose s = Queue.take s
end


(* module Q = QueueSet(IntCompare);; *)

(* Q.choose (Q.union (Q.singleton 4) (Q.insert 5 Q.empty)) *)

(* match (Q.choose (Q.union (Q.singleton 4) (Q.insert 5 Q.empty))) with
   | None -> None
   | Some (e, s) -> Q.choose s;; *)

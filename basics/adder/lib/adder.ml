(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let rec addlist l = match l with 
                [] -> 0
                | h :: l -> h + addlist l;;(* replace 0 with actual code *)

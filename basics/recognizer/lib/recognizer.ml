let rec lang1 w = match w with
                  [0] -> true
                | [1] -> true
                | [] -> false
                | h::w -> if h = 0 then lang1 w else if h = 1 then lang1 w else false;;

let rec lang2 w = match w with
                  [] -> false
                | [0] -> true
                | [1] -> true
                | h1::h2::w -> if h1 = 1 && h2 = 0 || h1 = 0 && h2 = 0 then false else lang2 (h2::w)
                | _ -> false;;

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  

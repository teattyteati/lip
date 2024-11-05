let rec lang1 w n = match w with
  [] -> false
| ['0'] -> true
| ['1'] -> true
| h::w -> if h = '0' then lang1 w n else if h = '1' then lang1 w n else false;;

let rec lang2 w n = match w with
    [] -> true
  | ['0'] -> true
  | ['1'] -> true
  | h1::h2::w -> if h1 = '1' && h2 = '0' || h1 = '0' && h2 = '0' then false else lang2 (h2::w) n
  | _ -> false;;

let rec lang3 w n = match w with
    [] -> false
  | h::[] -> if h = '0' && n > 0 then true else false
  | h::w -> if h = '0' && n = 0 then lang3 w (n+1) else if (h = '0' || h = '1') && n > 0 then lang3 w n else false;;

let rec lang4 w n = match w with
  [] -> if n = 2 then true else false
| h::w -> if h = '1' && n < 3 then lang4 w (n+1) else if h = '0' && n < 3 then lang4 w n else false;;

let rec lang5 w n = match w with
  [] -> false
| [_] -> false
| h1::h2::w -> if (h1 = '0' && h2 = '0') || (h1 = '1' && h2 = '1') then if w = [] then true else lang5 w n else false;;

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w 0) recognizers
  

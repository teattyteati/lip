open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl


let count current list =

  let counter i el = if el = current then i + 1 else i in

  List.fold_left counter 0 list
;;

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency num tokens =

  let uniq_list = List.sort_uniq compare tokens in


  List.map (fun current -> count current tokens) uniq_list
;;
(* frequency 3 [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")];; *)






(*contiamo le occurrences di elementi di una lista*)

let lista = [1;2;3;1;2;5];;

(*prima creiamo una lista con gli elementi da cercare*)

let uniq_elements = List.sort_uniq compare lista;;


let mapper uniq_element =
  let conta n elemento = if elemento = uniq_element then (n+1) else n in

  List.fold_left conta 0 lista;;
(*contiamo gli elementi*)
List.map mapper uniq_elements;;


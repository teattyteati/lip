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

let contatore current acc el = if current = el then acc+1 else acc;;

let c_occorr list current =
  List.fold_left (contatore current) 0 list;;

let unisci_liste e1 e2 = (e1, e2);;

let trim_condition num_to_take current _ = current < num_to_take;;

let condizione a b = match a with
(_, freq) -> match b with
              (_, freq2) -> compare freq2 freq
;;
(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n list = 
  let uniq_list = List.sort_uniq compare list in
  
  let count_list = List.map (c_occorr list) uniq_list in

  let frequency_list = List.map2 unisci_liste uniq_list count_list in

  let sorted_freq_list = List.sort condizione frequency_list in

  List.filteri (trim_condition n) sorted_freq_list;;

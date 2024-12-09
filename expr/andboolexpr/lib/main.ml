open Ast

let rec string_of_boolexpr = function
True -> "True" 
| False -> "False"
| If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
| Not (e0) -> "not "^(string_of_boolexpr e0)
| And (e0,e1) -> (string_of_boolexpr e0) ^ "and" ^ (string_of_boolexpr e1)
| Or (e0,e1) -> (string_of_boolexpr e0) ^ "or" ^ (string_of_boolexpr e1)

let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0, e1, e2) -> If(trace1 e0, e1, e2)
  (*| If(e3,_,_) -> trace1 e3 *)
  | Not(False) -> True
  | Not(True) -> False
  | Not(e0) -> Not(trace1 e0)

  | And(True,True) -> True
  | And(_,False)
  | And(False,_) -> False  (* qualsiasi espressione con uno dei due falso sarà falsa *)
  | And(e0,e1) -> And(trace1 e0,e1) (* altrimenti riduce l'albero (serve una virgola)*)

  | Or(True,_) 
  | Or(_,True) -> True    (* qualsiasi espressione con uno dei due vero sarà vera *)
  | Or(False,False) -> False
  | Or(e0,e1) -> Or(trace1 e0,e1) (* altrimenti riduce l'albero *)  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
True -> true
| False -> false
| If(e0,e1,e2) -> if (eval e0) then (eval e1) else (eval e2)
| Not(True) ->  false
| Not(False) -> true
| Not(e) -> not (eval e)
| And(e0,e1) -> (eval e0) && (eval e1)
| Or(e0,e1) -> (eval e0) || (eval e1) 

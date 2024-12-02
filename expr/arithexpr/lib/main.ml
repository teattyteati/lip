open Ast

let rec expr = function
  True -> "True" 
| False -> "False"
| If(e0,e1,e2) -> "If(" ^ (expr e0) ^ "," ^ (expr e1) ^ "," ^ (expr e2) ^ ")"
| Not (e0) -> "not "^(expr e0)
| And (e0,e1) -> (expr e0) ^ "and" ^ (expr e1)
| Or (e0,e1) -> (expr e0) ^ "or" ^ (expr e1)
| Zero -> "0"
| Succ (e0) -> "succ "^(expr e0)
| Pred (e0) -> "pred "^(expr e0)
| IsZero (e0) -> "iszero"^(expr e0)

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0, e1, e2) -> If(trace1 e0, e1, e2)
  (*| If(e3,_,_) -> trace1 e3 *)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


(* type exprval = Bool of bool | Nat of int *)
let rec eval = function
  True -> Bool true
| False -> Bool false
| If(Bool e0, e1, e2) -> if (eval e0) then (eval e1) else (eval e2)
| Not(e0) -> not (eval e0)
| And(e0,e1) -> (eval e0) && (eval e1)
| Or(e0,e1) -> (eval e0) || (eval e1)
| Succ(e0) -> Succ (eval e0)


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


  let string_of_val : exprval -> string = function
  | Bool true -> "True" 
  | Bool false -> "False"
  | Nat n -> string_of_int n

  let rec string_of_expr ast = match ast with
    True -> "True"
| False -> "False"
| If(e0, e1, e2) -> "If("^string_of_expr e0^","^string_of_expr e1^","^string_of_expr e2^")"

| Not(e) ->  "not "^string_of_expr e
| And(e0,e1) -> string_of_expr e0 ^ " and " ^ string_of_expr e1

| Or(e0,e1) -> string_of_expr e0 ^ " or " ^ string_of_expr e1


| Zero -> "Zero"

| Succ(e) -> "succ "^ string_of_expr e

| Pred(e) -> "pred "^ string_of_expr e


| IsZero(e) -> "IsZero "^ string_of_expr e



exception NoRuleApplies

let rec trace1 = function
   If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0, e1, e2) -> If(trace1 e0, e1, e2)

  | Not(False) -> True
  | Not(True) -> False
  | Not(e0) -> Not(trace1 e0)

  | And(True,True) -> True
  | And(_,False)
  | And(False,_) -> False  
  | And(e0,e1) -> And(trace1 e0,e1)

  | Or(True,_) 
  | Or(_,True) -> True
  | Or(False,False) -> False
  | Or(e0,e1) -> Or(trace1 e0,e1)

  | Succ e -> Succ (trace1 e)
  | Pred (Succ e) -> e
  | Pred e -> Pred (trace1 e)
  | IsZero Zero -> True
  | IsZero (Succ _ ) ->  False
  | IsZero e -> IsZero (trace1 e)
  | _ -> raise NoRuleApplies


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


(* type exprval = Bool of bool | Nat of int *)
let rec eval = function
  True -> Bool true
| False -> Bool false
| If(e0, e1, e2) -> (match eval e0 with
  | Bool true -> eval e1
  | Bool false -> eval e2
  | _ -> failwith "if su non booleani non supportato")

| Not(e) ->  (match eval e with
  | Bool e -> Bool (not e)
  | _ -> failwith "non possibile Not su non-Bool")

| And(e0,e1) -> (match eval e0, eval e1 with
  | Bool a, Bool b -> Bool (a && b)
  | _ -> failwith "And valuta solo su Bool")

| Or(e0,e1) -> (match eval e0, eval e1 with
  | Bool a, Bool b -> Bool (a || b)
  | _ -> failwith "Or valuta solo su Bool")

| Zero -> Nat 0

| Succ(e) -> (match eval e with
  | Nat x -> Nat (x+1)
  | _ -> failwith "successore non-Nat non possibile")

| Pred(e) -> (match eval e with
  | Nat 0 -> failwith "non possibile isZero di numeri negativi" 
  | Nat x -> Nat (x-1)
  | _ -> failwith "predecessore non-Nat non possibile")

| IsZero(e) -> (match eval e with 
  | Nat 0 -> Bool true 
  | Nat n -> if n>0 then Bool false else failwith "non possibile isZero di numeri negativi" 
  | _ -> failwith "non possibile isZero su non-Nat")


let is_nv e = match e with 
Zero -> true 
| Succ x -> (match (eval x) with
Nat _ -> true
| _ -> false)
| _ -> false
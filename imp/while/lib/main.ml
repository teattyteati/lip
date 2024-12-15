open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



let bind st x v : state = fun y -> if x = y then v else st y

(* let eval_expr : (state -> expr) -> exprval *)
let rec eval_expr st ex : exprval = match ex with
   True -> Bool true
  | False -> Bool false
  | Var (x) -> Nat (st x)
  | Const (x) -> Nat x
  | Not (x) -> (match (eval_expr st x) with
      Bool x -> Bool (not x)
    | Nat x -> failwith "NOT vuole un Bool."
  )
  | And (x, y) -> (match (eval_expr st x), (eval_expr st y) with
     Bool true, Bool true -> Bool true
    | Bool false, _ -> Bool false
    | _, Bool false -> Bool false
    | Nat x, _ -> failwith "Il primo elemento di un AND deve essere booleano."
    | _, Nat y -> failwith "Il secondo elemento di un AND deve essere booleano."
  )
  | Or (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Bool true, Bool y -> Bool true
    | Bool x, Bool true -> Bool true
    | Bool x , Bool y -> Bool false
    | Nat x, _ -> failwith "Il primo elemento di un OR deve essere booleano."
    | _, Nat y -> failwith "Il secondo elemento di un OR deve essere booleano."
  )
  | Add (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x+y)
    | Bool x, _ -> failwith "Il primo elemento di ADD non è Nat."
    | _, Bool x -> failwith "Il secondo elemento di ADD non è Nat."
  )
  | Sub (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x-y)
    | Bool x, _ -> failwith "Il primo elemento di SUB non è Nat."
    | _, Bool x -> failwith "Il secondo elemento di SUB non è Nat."
  )
  | Mul (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x*y)
    | Bool x, _ -> failwith "Il primo elemento di MUL non è Nat."
    | _, Bool x -> failwith "Il secondo elemento di MUL non è Nat."
  )
  | Eq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> if x = y then Bool true else Bool false
    | Bool x, _ -> failwith "Il primo elemento di EQ non è Nat."
    | _, Bool x -> failwith "Il secondo elemento di EQ non è Nat."
  )
  | Leq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> if x <= y then Bool true else Bool false
    | Bool x, _ -> failwith "Il primo elemento di LEQ non è Nat."
    | _, Bool x -> failwith "Il secondo elemento di LEQ non è Nat."
  )

(* let rec trace (i : int) (command : cmd) : conf list = try *)

(* trace1 : conf -> conf *)
let rec trace1 = function 
| St st -> St st
| Cmd (Skip, st) -> St st
| Cmd (Assign(x,y), st) -> St (trace1 (bind st x (eval_expr st y)))
| Cmd (Seq(x,y), st) -> (match x, y with 
  Cmd(x), Cmd(y) -> St (trace1 y St ((trace1 (Cmd(x), st))) st)
  | _, _ -> raise ( TypeError "Tipi sbagliati a SEQ.")
)
| Cmd(If (a,b,c), st) -> (match a,b,c with 
   Bool a, Cmd(b), Cmd(c) -> if a then St (trace1 b st) else St (trace1 c st)
  | a, Cmd(b), Cmd(c) -> St (trace1 If(eval_expr a, b, c) st)
  | _, _, _ -> raise (TypeError "Tipi sbagliati nel IF")
)
| Cmd(While(a, b), st) -> (match a,b with 
  Bool a, Cmd(b) -> if a then St (trace1 While(a, trace1 b st) st)
  | a, Cmd(b) -> St(trace1 While(eval_expr a, b) st)
)
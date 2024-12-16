open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* bind *)
let bind st x v : state = fun y -> if x = y then v else st y

(* let eval_expr : (state -> expr) -> exprval *)
let rec eval_expr (st:state) (ex:expr) : exprval = match ex with
    True -> Bool true
  | False -> Bool false
  | Var (x) -> st x
  | Const (x) -> Nat x
  | Not (x) -> (match (eval_expr st x) with
      Bool x -> Bool (not x)
    | Nat _ -> failwith "NOT vuole un Bool."
  )
  | And (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Bool true, Bool true -> Bool true
    | Bool false, _ -> Bool false
    | _, Bool false -> Bool false
    | Nat _, _ -> failwith "Il primo elemento di un AND deve essere booleano."
    | _, Nat x -> failwith (("Il secondo elemento di un AND deve essere booleano. ") ^ (string_of_int x))
  )
  | Or (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Bool true, Bool _ -> Bool true
    | Bool _, Bool true -> Bool true
    | Bool _ , Bool _ -> Bool false
    | Nat _, _ -> failwith "Il primo elemento di un OR deve essere booleano."
    | _, Nat _ -> failwith "Il secondo elemento di un OR deve essere booleano."
  )
  | Add (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x+y)
    | Bool _, _ -> failwith "Il primo elemento di ADD non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di ADD non è Nat."
  )
  | Sub (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x-y)
    | Bool _, _ -> failwith "Il primo elemento di SUB non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di SUB non è Nat."
  )
  | Mul (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> Nat (x*y)
    | Bool _, _ -> failwith "Il primo elemento di MUL non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di MUL non è Nat."
  )
  | Eq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> if x = y then Bool true else Bool false
    | Bool _, _ -> failwith "Il primo elemento di EQ non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di EQ non è Nat."
  )
  | Leq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Nat x, Nat y -> if x <= y then Bool true else Bool false 
      (* Nat x, Nat y -> failwith ((string_of_int x)^(string_of_int y)) *) 
    | Bool _, _ -> failwith "Il primo elemento di LEQ non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di LEQ non è Nat."
  )

(* let rec trace (i : int) (command : cmd) : conf list = try *)

(* trace1 : conf -> conf *)
let rec trace1 (c : conf) : conf = match c with 
  St st -> St st
| Cmd (Skip, st) -> St st
| Cmd (Assign(x,y), st) -> St (bind st x (eval_expr st y))
| Cmd (Seq(x,y), st) -> (match trace1 (Cmd(x, st)) with
    St st -> (Cmd (y, st))
  | Cmd(x', st) -> (Cmd(Seq(x', y), st))
)

| Cmd(If (a,b,c), st) -> (match (eval_expr st a) with
    Bool true -> (Cmd(b, st))
  | Bool false -> (Cmd(c, st))
  | Nat _ -> failwith "problem?")

| Cmd(While(a, b), st) -> (match (eval_expr st a) with
    Bool true -> (Cmd(Seq(b, While(a, b)), st))
  | Bool false -> St st
  | Nat _ -> failwith "problem?")

(* bottom : state *)
let (bottom:state) = fun _ -> raise (UnboundVar "Variabile non trovata.")
let (confl: conf list) = []


let rec helper (i:int) (c:conf) : conf list =
  match trace1 c with
    x -> (match i with
           0 -> []
          |i-> x::helper (i-1) x)
    


(* trace : int -> cmd -> conf list *)
let trace (i:int) (c:cmd) : conf list = helper i (Cmd(c, bottom))
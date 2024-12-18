open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* bind *)
(*let bind st x v : state = fun y -> if x = y then v else st y*)
(* eval_expr : state -> expr -> memval *)

let rec eval_expr (st:state) (ex:expr) : memval = match ex with
    True -> Bool true
  | False -> Bool false
  | Var (x) -> (match topenv st x with
                    BVar y
                  | IVar y -> getmem st y
                )
  | Const (x) -> Int x
  | Not (x) -> (match (eval_expr st x) with
      Bool x -> Bool (not x)
    | Int _ -> failwith "NOT vuole un Bool."
  )
  | And (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Bool true, Bool true -> Bool true
    | Bool false, _ -> Bool false
    | _, Bool false -> Bool false
    | Int _, _ -> failwith "Il primo elemento di un AND deve essere booleano."
    | _, Int x -> failwith (("Il secondo elemento di un AND deve essere booleano. ") ^ (string_of_int x))
  )
  | Or (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Bool true, Bool _ -> Bool true
    | Bool _, Bool true -> Bool true
    | Bool _ , Bool _ -> Bool false
    | Int _, _ -> failwith "Il primo elemento di un OR deve essere booleano."
    | _, Int _ -> failwith "Il secondo elemento di un OR deve essere booleano."
  )
  | Add (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Int x, Int y -> Int (x+y)
    | Bool _, _ -> failwith "Il primo elemento di ADD non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di ADD non è Nat."
  )
  | Sub (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Int x, Int y -> Int (x-y)
    | Bool _, _ -> failwith "Il primo elemento di SUB non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di SUB non è Nat."
  )
  | Mul (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Int x, Int y -> Int (x*y)
    | Bool _, _ -> failwith "Il primo elemento di MUL non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di MUL non è Nat."
  )
  | Eq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Int x, Int y -> if x = y then Bool true else Bool false
    | Bool _, _ -> failwith "Il primo elemento di EQ non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di EQ non è Nat."
  )
  | Leq (x, y) -> (match (eval_expr st x), (eval_expr st y) with
      Int x, Int y -> if x <= y then Bool true else Bool false 
      (* Nat x, Nat y -> failwith ((string_of_int x)^(string_of_int y)) *) 
    | Bool _, _ -> failwith "Il primo elemento di LEQ non è Nat."
    | _, Bool _ -> failwith "Il secondo elemento di LEQ non è Nat."
  )

(* eval_decl : state -> decl list -> state *)

(* mette troppi stati uno sull'altro  *)

let rec help_decl (env:env) (loc:loc) (decl_list:decl list) : (env*loc) =
  match decl_list with
    h::l -> (match h with 
                IntVar(x) -> help_decl (bind_env (env) (x) (IVar((loc)))) (loc+1) l
              | BoolVar(x) -> help_decl (bind_env (env) (x) (BVar((loc)))) (loc+1) l
              )
  | [] -> (env, loc)
let eval_decl (st:state) (decl_list:decl list) : state =
  match help_decl (topenv st) (getloc st) decl_list with
   (env, loc) -> make_state (env :: getenv st) (getmem st) (loc)
  

(* trace1 : conf -> conf *)
let rec trace1 (c : conf) : conf = match c with 
  Cmd (Skip, st) -> St st
| Cmd (Assign(x,y), st) -> (match (topenv st) x with
                            BVar(v) -> St (make_state (getenv st) (bind_mem (getmem st) (v) (eval_expr st y)) (getloc st))
                          | IVar(v) -> St (make_state (getenv st) (bind_mem (getmem st) (v) (eval_expr st y)) (getloc st)))
| Cmd (Seq(x,y), st) -> (match trace1 (Cmd(x, st)) with
    St st -> (Cmd (y, st))
  | Cmd(x', st) -> (Cmd(Seq(x', y), st))
)

| Cmd(If (a,b,c), st) -> (match (eval_expr st a) with
    Bool true -> (Cmd(b, st))
  | Bool false -> (Cmd(c, st))
  | _ -> failwith "problem?")

| Cmd(While(a, b), st) -> (match (eval_expr st a) with
    Bool true -> (Cmd(Seq(b, While(a, b)), st))
  | Bool false -> St st
  | _ -> failwith "problem?")

| Cmd(Decl(l,b), st) -> Cmd(Block(b), (eval_decl st l))
| Cmd(Block(b), st) -> (match (trace1 (Cmd(b, st))) with
                         St st -> St (make_state (popenv st) (getmem st) (getloc st))
                       | Cmd(b, st) -> Cmd(Block(b), st)
                       )
                       
| St st -> St st


(* bottom : state *)
(* let (bottom:state) = fun _ -> raise (UnboundVar "Variabile non trovata.") *)


let rec helper (i:int) (c:conf) : conf list =
  match trace1 c with
    x -> (match i with
           0 -> []
          |i-> x::helper (i-1) x)
    


(* trace : int -> cmd -> conf list) *)
let trace (i:int) (c:cmd) : conf list = helper i (Cmd(c, state0))
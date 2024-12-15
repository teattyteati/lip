open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast



let bind st x v : state = fun y -> if x = y then v else st y



(* let rec trace (i : int) (command : cmd) : conf list = try *)
  

(* let eval_expr : (state -> expr) -> exprval *)
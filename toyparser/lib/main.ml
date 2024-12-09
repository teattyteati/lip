open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = (int, string) Result.t

let ( ==> ) (res : result) (f : int -> result) : result =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_result : result -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

(* eval : ast -> result *)

let rec eval (flag : bool) (ast : ast) : result =
  match ast, flag with

  | Const n, _ -> Ok n
  (*| Const n, true -> if n = 0 then Error ("Error: tried to divide "^string_of_int n^" by zero") else Ok n*)
  
  | Add (e1,e2), _ ->
    let res1 = eval false e1 in
    let res2 = eval false e2 in
    begin 
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 + v2)
    end
    
  | Sub (e1,e2), _ ->
    let res1 = eval false e1 in
    let res2 = eval false e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 - v2)
    end

  | Mul (e1,e2), _ ->
    let res1 = eval false e1 in
    let res2 = eval false e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 * v2)
    end

  | Div (e1,e2), _ ->
    let res1 = eval false e1 in
    let res2 = eval true e2 in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> if(v2=0) then Error ("Error: tried to divide "^ string_of_int v1 ^" by zero") else Ok (v1 / v2) end
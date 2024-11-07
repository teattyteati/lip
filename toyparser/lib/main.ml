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

let rec eval : ast -> bool -> result = function

  | Const n -> Ok n
  
  | Add (e1,e2) ->
    let res1 = eval e1 false in
    let res2 = eval e2 false in
    begin 
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 + v2)
    end
    
  | Sub (e1,e2) ->
    let res1 = eval e1 false in
    let res2 = eval e2 false in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 - v2)
    end

  | Mul (e1,e2) ->
    let res1 = eval e1 false in
    let res2 = eval e2 false in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 * v2)
    end

  | Div (e1,e2) ->
    let res1 = eval e1 false in
    let res2 = eval e2 false in
    begin
    match res1, res2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 -> Ok (v1 / v2)
    end
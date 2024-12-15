{
    open Parser
}

let white = [' ' '\t']+
let const = ['0'-'9']*
let var = ['a'-'z''A'-'Z''_']*

rule read =
    parse
    | white { read lexbuf }
    | "true" { TRUE }
    | "false" { FALSE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | "+" { ADD }
    | "-" { SUB }
    | "*" { MUL }
    | "=" { EQ }
    | "<=" { LEQ }
    | "skip" { SKIP }
    | ":=" { ASSIGN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "while" { WHILE }
    | "do" { DO }
    |";" { SEQ }
    | var { VAR (Lexing.lexeme lexbuf) }
    | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }

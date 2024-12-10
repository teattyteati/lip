{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "or" { OR }
  | "and" { AND }
  | "iszero" { ISZERO }
  | "not" { NOT }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | eof { EOF }

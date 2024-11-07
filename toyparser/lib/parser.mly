%{
open Ast
%}

%token <string> CONST
%token MUL
%token DIV
%token PLUS
%token SUB

%token LPAREN
%token RPAREN
%token EOF

%left PLUS SUB
%left MUL DIV

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e3 = expr; SUB e4 = expr { Sub(e3, e4) }
  | e4 = expr; MUL e5 = expr { Mul(e4, e5) }
  | e5 = expr; DIV e6 = expr { Div(e5, e6) }
  | LPAREN; e=expr; RPAREN {e}
;

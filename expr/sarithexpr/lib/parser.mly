%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token NOT
%token THEN
%token ELSE
%token EOF
%token OR
%token AND
%token ZERO
%token SUCC
%token PRED
%token ISZERO

%left OR AND PRED SUCC
%right NOT ISZERO

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e1 = expr; AND; e2 = expr; { And (e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or (e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
  | ZERO; { Zero }
  | SUCC; e1 = expr; { Succ (e1) }
  | PRED; e1 = expr; { Pred (e1) }
  | ISZERO; e1 = expr; { IsZero (e1) }
  | NOT; e1 = expr; { Not (e1) }
;

%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> VAR
%token <int> CONST
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token EOF
%token RPAREN
%token LPAREN


%left SEQ
%left DO ELSE EQ

%start <cmd> prog

%%

prog:
  | e = cmd; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | e1 = expr; AND; e2 = expr; { And (e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or (e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
  | NOT; e = expr; { Not (e) }
  | e1 = expr; ADD; e2 = expr { Add (e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub (e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq (e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq (e1, e2) }
  | e = VAR { Var (e) }
  | e = CONST { Const (e) }
;

cmd:
  | IF; e1 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e1, c1, c2) }
  | SKIP { Skip }
  | e1 = cmd; SEQ; e2 = cmd { Seq (e1, e2) }
  | e1 = VAR; ASSIGN; e2 = expr { Assign (e1, e2) }
  | WHILE; e = expr; DO; c = cmd { While (e, c) }


(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | TRUE
  | THIS
  | STAR
  | SEMI
  | RPAR
  | RETURN
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEW
  | NEQUAL
  | N of (int)
  | MOD
  | MINUS
  | MAIN
  | LPAR
  | LOWER
  | LEQUAL
  | INT
  | IF
  | IDENT of (string)
  | GREATER
  | GEQUAL
  | FALSE
  | EXTENDS
  | EQUAL
  | EOF
  | END
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
  | BOOL
  | BEGIN
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)

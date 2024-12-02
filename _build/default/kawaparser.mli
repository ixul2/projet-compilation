
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VAR
  | TRUE
  | THIS
  | STAR
  | SEMI
  | RPAR
  | RETURN
  | PRINT
  | PLUS
  | OR
  | NEW
  | NEQUAL
  | N of (int)
  | MINUS
  | METHOD
  | MAIN
  | LPAR
  | LOWER
  | LEQUAL
  | INT
  | IF
  | IDENT of (string)
  | FALSE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DIV
  | CLASS
  | BOOL
  | BEGIN
  | ATTRIBUTE
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)

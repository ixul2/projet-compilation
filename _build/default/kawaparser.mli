
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VAR
  | TRUE
  | THIS
  | STAR
  | SEMI
  | R_BRACKET
  | RPAR
  | RETURN
  | PRINT
  | PLUS
  | OR
  | NOT
  | NEW
  | NEQUAL_STRUCT
  | NEQUAL
  | N of (int)
  | MOD
  | MINUS
  | METHOD
  | MAIN
  | L_BRACKET
  | LPAR
  | LOWER
  | LEQUAL
  | INT
  | IF
  | IDENT of (string)
  | GREATER
  | GEQUAL
  | FINAL
  | FALSE
  | EXTENDS
  | EQUAL_STRUCT
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
  | ATTRIBUTE
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)


(* The type of tokens. *)

type token = 
  | WHILE
  | THEN
  | STAR
  | SPAWN
  | SET
  | SEMI
  | RP
  | REF
  | PLUS
  | MINUS
  | LP
  | LET
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | FUN
  | EQ
  | EOF
  | ELSE
  | DONE
  | DO
  | BANG
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)

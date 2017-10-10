type instruction =
  (* Fragment A*)
  | Int of int
  | Lookup of string
  | Add | Sub | Mult
  | Let of string
  | EndLet of string
(* Fragment F*)
  | MkClos of string * block
  | Return
  | Apply
and block = instruction list

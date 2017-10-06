type instruction =
  (* Fragment A*)
  | Int of int
  | Lookup of string
  | Add
  | Let of string
  | EndLet of string
(* Fragment F*)
  | MkClos of string * block
and block = instruction list

type instruction =
  (* Fragment A*)
  | Int of int
  | Lookup of string
  | Binop of binop
  | Let of string
  | EndLet of string
(* Fragment F*)
  | MkClos of string * block
  | Return
  | Apply
(* Fragment M *)
  | Alloc
  | Store
  | Load
  | Dup
  | Drop
  | Unit
 (* Fragment C *)
  | Spawn
 (* Extention *)
  | Cond of block * block
  | Loop of block * block
and block = instruction list
and binop = Add | Sub | Mult

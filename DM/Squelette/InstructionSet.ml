type instruction =
  (* Fragment A*)
  | Int of int
  | Bool of bool
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
  | StoreLock  of block * block * right
  | Load
  | CompSwap
  | Dup
  | Drop
  | Unit
 (* Fragment C *)
  | Spawn
 (* Extention *)
  | Cond of block * block
  | Loop of block * block
  | Show of int
  | Wait
  | Join
and block = instruction list
and right = Free | Share | Lock
and binop =  Add | Sub | Mult | Div
             | Or | And
             | Gt | Lt | Ge | Le
             | Eq | Neq | Eqphy




type expr =
  | Int   of int
  | Bool  of bool
  | Ident of ident
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Letin of ident * expr * expr
  | Fun   of ident * expr
  | Apply of expr * expr
  | Cond  of expr * expr * expr
  | Loop  of expr * expr
  | Seq   of expr * expr
  | Ref   of expr
  | GetR  of expr
  | SetR  of expr * expr
  | Spawn of expr * expr
  (* ---Ajout--- *)
  | Show of expr list
  | Wait                        
  | Join
and ident = string
and binop = Add | Sub | Mult
            (* ---Ajout--- *)
            | Div
            | Or | And
            | Gt | Lt | Ge | Le
            | Eq | Neq | Eqphy 
and unop = Minus
         | Not



type t = Tint
       | Tbool
       | Tident
       | Tfun
       | Tref

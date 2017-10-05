type instruction =
  | Int of int
  | Lookup of string
  | Add
  | Let of string
  | EndLet of string
and block = instruction list

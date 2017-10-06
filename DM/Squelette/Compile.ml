open InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
     [Int(n)]

  | Ast.Ident(id) ->
     [Lookup(id)]

  | Ast.Binop(Ast.Add, e1, e2) ->
     (* D'abord un opérande, puis
	l'autre, puis l'opérateur,
	comme en notation
	polonaise inversée. *)
     (compile_expr e2)
     @ (compile_expr e1)
     @ [Add]
  | Ast.Letin(id, e1, e2) ->
     (compile_expr e1)
     @ [Let(id)]
     @ (compile_expr e2)
  | _ -> failwith "Not implemented"

(* let x = 4 in x + 2 *)

(* 
   @ [Int(4)]
   @ [Let("x")]
   @ [Binop(Add, Ident("x"), Int(2))]
   @ [Lookup("x")] 
*)

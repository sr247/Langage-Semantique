module IS = InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
     [IS.Int(n)]

  | Ast.Ident(id) ->
     [IS.Lookup(id)]

  | Ast.Binop(op, e1, e2) ->
     let isop = 
       match op with
       | Ast.Add -> IS.Add
       | Ast.Sub -> IS.Sub
       | Ast.Mult -> IS.Mult
     (* D'abord un opérande, puis
	l'autre, puis l'opérateur,
	comme en notation
	polonaise inversée. *)
     in
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Binop(isop)]
       
  | Ast.Letin(id, e1, e2) ->
     (compile_expr e1)
     @ [IS.Let(id)]
     @ (compile_expr e2)       
       (** expl :let x = 4 in x + 2
	   
	   @ [Int   (4)]
	   @ [Let("x")]
	   @ [Binop(Add, Ident("x"), Int(2))]
	   @ [Lookup("x")] 
	   
       **)
       
  | Ast.Fun(id, c) ->
     let c' = compile_expr c in
     [IS.MkClos(id, c'@[IS.Return])]
       
  | _ -> failwith "Not implemented"
  


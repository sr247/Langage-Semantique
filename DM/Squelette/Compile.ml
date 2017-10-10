module IS = InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
     [IS.Int(n)]

  | Ast.Ident(id) ->
     [IS.Lookup(id)]

  | Ast.Binop(Ast.Add, e1, e2) ->
     (* D'abord un opérande, puis
	l'autre, puis l'opérateur,
	comme en notation
	polonaise inversée. *)
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Add]
  | Ast.Binop(Ast.Mult, e1, e2) ->
     (* D'abord un opérande, puis
	l'autre, puis l'opérateur,
	comme en notation
	polonaise inversée. *)
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Mult]
  | Ast.Binop(Ast.Sub, e1, e2) ->
     (* D'abord un opérande, puis
	l'autre, puis l'opérateur,
	comme en notation
	polonaise inversée. *)
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Sub]
         
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
       
  | Ast.Apply(t, e) ->
     let c' = compile_expr e in
     [compile_expr e]
     @ [IS.Apply]
  (** expl: f e

  compile_expr e
  @ compile_expr f

   *)
  | _ -> failwith "Not implemented"
  


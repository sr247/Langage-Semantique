module IS = InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
     [IS.Int(n)]
  | Ast.Bool(b) ->
     [IS.Bool(b)]

  | Ast.Ident(id) ->
     [IS.Lookup(id)]

  | Ast.Binop(op, e1, e2) ->
     let isop = 
       match op with
       | Ast.Add  -> IS.Add
       | Ast.Sub  -> IS.Sub
       | Ast.Mult -> IS.Mult
       | Ast.Div  -> IS.Div
       | Ast.Gt -> IS.Gt
       | Ast.Lt -> IS.Lt
       | Ast.Ge -> IS.Ge
       | Ast.Le -> IS.Le
       | Ast.Eq -> IS.Eq
       | Ast.Neq -> IS.Neq
       | Ast.Eqphy -> IS.Eqphy
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
     @ [IS.EndLet(id)]
       
  | Ast.Fun(id, c) ->
     let c' = compile_expr c in
     [IS.MkClos(id, c'@[IS.Return])]

  | Ast.Apply(e1, e2) ->
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Apply]

  (* Fragment M *)
  | Ast.Ref(e) ->
     [IS.Alloc]
     @ [IS.Dup]
     @ compile_expr e
     @ [IS.Store]

  | Ast.GetR(e) ->
     compile_expr e
     @ [IS.Load]
       
  | Ast.Seq(e1, e2) ->
     compile_expr e1
     @ [IS.Drop]
     @ compile_expr e2
       
  | Ast.SetR(d, e) ->
     [IS.Atom]
     @ (compile_expr d)
     @ compile_expr e
     @ [IS.Store]
     @ [IS.Unit]
       
  (* Fragment C *)
  | Ast.Spawn(e1, e2) ->
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Spawn]
     @ [IS.Unit]
       
  (* Extention *)
  | Ast.Cond(c, e1, e2) ->
     let cc = compile_expr c in
     let ce1 = compile_expr e1 in
     let ce2 = compile_expr e2 in
     cc @ [IS.Cond(ce1,ce2)]
       
  | Ast.Loop(c, e) ->
     let cc = compile_expr c in
     let ce = compile_expr e in
     cc @ [IS.Loop(cc, ce)]
       
  | Ast.Show(el) ->
     (List.flatten (List.map (fun e -> compile_expr e) el))
     @ [IS.Show(List.length el); IS.Unit]
       
  (* Le main attend la fin des autres thread*)
  | Ast.Wait ->
     [IS.Wait]
  (* Le main Wait + récupère les valeurs des threads *)
  | Ast.Join ->
     [IS.Join]
  | _ -> failwith "Compile::compile_expr::Not implemented"

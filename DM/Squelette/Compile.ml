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

  | Ast.Apply(e1, e2) ->
     (compile_expr e2)
     @ (compile_expr e1)
     @ [IS.Apply]

  (* Fragment M *)
  |Ast.Ref(e) ->
    [IS.Alloc]
    @[IS.Dup]
    @compile_expr e
    @[IS.Store]

  |Ast.GetR(e) ->
    compile_expr e
    @[IS.Load]
       
  | Ast.Seq(e1, e2) ->
     compile_expr e1
     @[IS.Drop]
     @compile_expr e2
                   
  | Ast.SetR(d, e) ->
     compile_expr d
     @compile_expr e
     @[IS.Store]
     @[IS.Unit]
        
  (* Fragment C *)
  | Ast.Spawn(e1, e2) ->
     (compile_expr e2)
     @(compile_expr e1)
     @[IS.Spawn]        
     @[IS.Unit]
          
  (* Extention *)
  | Ast.Cond(c, e1, e2) ->
     let cc = compile_expr c in
     let ce1 = compile_expr e1 in
     let ce2 = compile_expr e2 in
     cc
     @[IS.Cond(ce1,ce2)]
  | Ast.Loop(c, e) ->
     let cc = compile_expr c in
     let ce = compile_expr e in
     cc
     @[IS.Loop(cc, ce)]
  | _ -> failwith "Not implemented"

open Ast

type typed_expr = {expr: Ast.expr;
                   t:Ast.t}

type typed_ast = T of typed_expr


let rec get_type e =
  match e with
  | Int(n) -> Tint
  | Bool(b) -> Tbool
  | Ident(id) -> Tident
  | Fun(id, e) -> Tfun
  | Ref(e) -> Tref
  | Unop(op, e) -> get_type e
  | Binop(op, e1, e2) ->
     let t1,t2 = get_type e1, get_type e2 in
     if t1 = t2 then t1 else failwith"Type::get_type::Wrong type"
  | _ ->
     failwith "Type::get_type:: Not implemented"

let mk_unop op e =
  begin
     let ty = get_type e in
     match op with
     | Minus ->
        begin
          match ty with
          | Tint | Tident -> Unop(op, e)
          | _ -> failwith "Type::Expression has to be int"
        end
     | Not ->
        begin
          match ty with
          | Tbool| Tident -> Unop(op, e)
          | _ -> failwith "Type::Expression has to be bool"
        end
     end
    
let mk_binop op e1 e2 =
  begin
    let t1, t2 = get_type e1, get_type e2 in
    match op with
    | Add
    | Sub
    | Mult
    | Div ->
       begin
         match t1, t2 with
         | Tint, Tint -> Binop(op, e1, e2)
         | _ ->
            failwith "Type::Expression 
                      has to be int -> int"
       end
    | Or
    | And ->
       begin
         match t1, t2 with
         | Tbool, Tbool -> Binop(op, e1, e2)
         | _ ->
            failwith "Type::Expression 
                      has to be bool -> bool"
       end
    | Gt | Lt | Ge | Le
    | Eq | Neq
    | Eqphy ->
       begin
         match t1, t2 with
         | Tint, Tint
         | Tbool, Tbool ->
            Binop(op, e1, e2)
         | _, _ ->
            (* Cas de l'égalité structurel 
             Eventuellement pour fun etc...*)
            failwith "Type::Expression 
                      has to be 'a -> 'a"
       end
  end

let mk_apply e1 e2 =
  match e1, e2 with
  | Fun(id, e), e2  -> Apply(e1, e2)
    
let rec check_type ast =
  match ast with
  (* let e = {expr=Bool(b); t=Tbool} in *)
  (* T(e) *)
  | Int(n) -> ast     
  | Bool(b) -> ast
  | Ident(id) -> ast
  | Fun(id, e) -> ast
  | Letin(id, e1, e2) -> ast
  | Unop(op, e) as expr ->
     mk_unop op e
  | Binop(op, e1, e2) as expr ->
     mk_binop op e1 e2
  | Apply(t, u) ->
     mk_apply t u
  | _ -> ast


  (* | _ -> check_operand ast *)
(* | Apply(t u) ->  *)
(* | Cond(c, e1, e2) -> *)
(* | Loop(c, e2) -> *)
(* | Seq(e1, e2) -> *)
(* | Ref(e) -> *)
(* | GetR(e) -> *)
(* | SetR(e1, e2) -> *)
(* | Spawn(e1, e2) -> *)
(* (\* ---Ajout--- *\) *)
(* | Show(el) -> *)
(* | Wait -> Wait  *)
(* | Join -> Join *)
(* and check_operand op e1 e2 = *)
(*   match op with *)
(*   | Add *)
(*   | Sub *)
(*   | Mult *)
(*   | Div -> *)
(*      begin *)
(*        match e1,e2 with *)
(*        | Int(_), Int(_) -> *)
(*           Printf.printf "Typed !"; *)
(*           Binop(op, e1, e2) *)
(*        | _, _ -> *)
(*           let e1 = e1 in *)
(*           let e2 = e2 in *)
(*           Binop(op, e1, e2) *)
(*                 failwith"Expression type expected is int" *)
(*      end *)
(*   | Or *)
(*   | And -> *)
(*      begin *)
(*        match e1,e2 with *)
(*        | Bool(_), Bool(_) -> expr *)
(*        | _, _ -> *)
(*           failwith"Expression type expected is bool" *)
(*      end *)
(*   | Gt | Lt *)
(*   | Ge | Le *)
(*   | Eq | Neq *)
(*   | Eqphy -> *)
(*      begin *)
(*        match e1,e2 with *)
(*        | Int(_), Int(_) -> expr *)
(*        | Bool(_), Bool(_) -> expr *)
(*        | _, _ -> *)
(*           failwith"Expression type expected is 'a -> 'a" *)
(*      end                        *)
(* end      *)
(*    | _ -> expr *)
(* (\* | Apply(t u) -> *\) *)
(* (\* failwith "Type::check_operand::Not Implemented" *\) *)
            

open Ast


let rec check_operand expr =
  match expr with
  | Binop(op, e1, e2) ->
     begin
       match op with
       | Add
       | Sub
       | Mult
       | Div ->
          begin
            match e1,e2 with
            | Int(_), Int(_) -> expr
            | _, _ ->
               failwith"Expression type expected is int" end             
       | Or
       | And ->
          begin
            match e1,e2 with
            | Bool(_), Bool(_) -> expr
            | _, _ ->
               failwith"Expression type expected is bool"
          end
       | Gt | Lt
       | Ge | Le
       | Eq | Neq
       | Eqphy ->
          begin
            match e1,e2 with
            | Int(_), Int(_) -> expr
            | Bool(_), Bool(_) -> expr
            | _, _ ->
               failwith"Expression type expected is 'a -> 'a"
          end                       
     end     
  | _ ->
     failwith "Type::check_operand::Not Implemented"

                     
let rec check_type ast =
  match ast with
  | Int(_) | Bool(_)
  | Ident(_) as expr -> expr
  | Unop(_, _) as expr-> expr   (* Va falloir vérifier pas mal de truc ici *)
  | Letin(_, _, _) 
  | Fun(_, _) as expr-> expr
  | _ -> check_operand ast
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
  (* | Wait -> Wait                         *)
  (* | Join -> Join *)

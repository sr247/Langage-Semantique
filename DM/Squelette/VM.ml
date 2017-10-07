open Printf
module IS = InstructionSet

type value =
  | Int of int
             
module Env = Map.Make(String)
type env = value Env.t

(* Ici, version immuable *)
(*
  type thread_state = {
  code  : block;
  stack : value list;
  env   : env
  }
 *)

type thread_state = {
  mutable code  : IS.block;
  mutable stack : value list;
  mutable env   : env
}

exception End_of_thread of thread_state
exception NF_Env of string
let step state =
  let fetch() =
    match state.code with
    | []   ->
       raise (End_of_thread state)
    | i::c ->
       state.code <- c;
       i
  in
  let push v =
    state.stack <- v::state.stack
  in
  let pop() =
    match state.stack with
    | [] -> assert false
    | v::s ->
       state.stack <- s;
       v
  in
  match fetch() with
  (* Fragment A *)
  | IS.Int(n) ->
     push (Int n)
  | IS.Lookup(id) ->
     let v =
       try
	 Env.find id state.env
       with Not_found -> raise (NF_Env id)
     in
     push v
  | IS.Add ->
     let Int n1 = pop () in
     let Int n2 = pop () in
     push(Int(n1+n2))
  | IS.Let(id) ->
     let v = pop () in
     state.env <- (Env.add id v state.env)
  | IS.EndLet(id) ->
     state.env <- (Env.remove id state.env)
  (* Fragment F *)
  | IS.MkClos(id, c) -> ()
  | _ -> failwith "Not implemented"
(*  *)
let execute p : unit =
  let b = {code=p; stack=[]; env=Env.empty }
  in 
  let rec exec state =

    step state; exec state
  in
  try
    exec b
  with End_of_thread(state) ->
       match state.stack with
       | Int(n)::s -> printf "%d\n" n

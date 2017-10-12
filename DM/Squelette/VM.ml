open Printf
module IS = InstructionSet

type value =
  | Int of int
  | Closure of string * IS.block
             
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
exception Not_found_in_Env of string
                      
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
       state.stack <- s; v
  in
  match fetch() with
  (* Fragment A *)
  | IS.Int(n) ->
     push (Int n)
  | IS.Lookup(id) ->
     let v =
       try
	 Env.find id state.env
       with Not_found -> raise (Not_found_in_Env id)
     in
     push v
  | IS.Binop(op) ->
     begin
       match op with       
       | IS.Add ->
	  let Int n1 = pop () in
	  let Int n2 = pop () in
	  push(Int(n1+n2))
       | IS.Sub ->
	  let Int n1 = pop() in
	  let Int n2 = pop() in
	  push(Int(n1-n2))
       | IS.Mult ->
	  let Int n1 = pop() in
	  let Int n2 = pop() in
	  push(Int(n1*n2))
     end
  | IS.Let(id) ->
     let v = pop () in
     state.env <- (Env.add id v state.env)
  | IS.EndLet(id) ->
     state.env <- (Env.remove id state.env)
  (* Fragment F *)
  | IS.MkClos(id, c') ->
     let clos = Closure(id, c') in
     state.env <- (Env.add id clos state.env);
       push (clos)
  | IS.Apply ->
     let Closure(id, c') = pop () in
     let f =
       try
	 Env.find id state.env
       with Not_found -> raise (Not_found_in_Env id)
     in
     state.code <- c'@state.code
       
  | _ -> failwith "Not implemented"

(**
   A function which print recursively all the element 
   contained in a Closure term.
*)
let rec print_clos id c = 
  printf "(%s : fun -> " id;
  List.iter (fun inst ->
    match inst with
    | IS.Int(i) -> print_int i
    | IS.Lookup(id) -> printf " %s " id
    | IS.Binop(op) ->
       begin
    	 match op with
    	 | IS.Add -> printf " + "
    	 | IS.Sub -> printf " - "
    	 | IS.Mult -> printf " * "
       end
    | IS.Let (id) -> printf " %s " id
    | IS.MkClos(id, c) -> print_clos id c; printf ")"; 
    | _ -> ()
  ) c

(**
   A fonction which executes the list code "p" retreived from
   the compiling pass and then initiates a "b" state in which:

   b.code = the p list instruction
   b.stack = the stack execution initiate at empty
   b.env = the environment initiate at empty
   
   The function executes instructions by instructions 
   the b.code (using step function) and changes the 
   state of b.stack and the environment.
   
*)
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
    | Closure(id, c)::s -> print_clos id c;
      printf ")\n"
	 


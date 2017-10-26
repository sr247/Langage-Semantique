open Printf
module IS = InstructionSet

module Env = Map.Make(String)
type env = value Env.t
and value =
    | Int of int
    | Closure of string * IS.block * env



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
(* Ajout : list de thread + thread courant *)
}
let heap = []
(* L'état de la mémoire *)
  
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
     let v = Closure(id, c', state.env) in
     push(v)
  | IS.Apply ->
     let Closure(id, c', e') = pop() in
     let v = pop() in
     push (Closure("main", state.code, state.env));
     state.code <- c';
     state.env <- (Env.add id v e');
  | IS.Return ->
     let v = pop() in
     let Closure(id, c', e') = pop()
       (* match pop() with *)
       (* | Closure("main", c', e') as ret-> ret *)
       (* | _ -> failwith "Closure Main Expected" *)
     in
     if id = "main" then
       begin
	 state.stack <- v::state.stack;
	 state.code <- c';
	 state.env <- (Env.remove id e')
       end
     else
       failwith "not a return"
       
  | _ -> failwith "Not implemented"

(**
   A function which print recursively all the element 
   contained in a Closure term.
*)
let rec print_clos id c = 
  printf "(fun %s -> " id;
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
    | Closure(id, c, _)::s -> print_clos id c;
      printf ")\n"
	 



	(* fun x -> x, (fun y -> 6*y) 3, let f = fun x -> 2*x in ()  *)

	(** Quelques mots sur Spawn:
	    rappel de yield : file contenant les threads
	    Thread s'exécutant jusqu'à son instruction le mettant dans la file.
	    
	    Ici Chaque threads à son propre env et sa propre stack
	    Mais ils partagent la meme mémoire heap.
	    Ordonancer avec un aléatoire suffisement grand pour le nombre de pas de réduction (1-10)
	    
	    
  *)
	(** Typage des exceptions
	    try e1 catch e2 = match ee1 with
	    |E -> e2
	    |V(v1) -> V(v1)

	    dire qu'un terme est bien type -> E[Vide] |- e: T
	    e a le type T dans l'environnemnt vide
	    
	    Alors [e] est bien typée -> Il existe T', E[Vide] |- [e] : T'
	    T' est de la forme () + T
	    
	    Par récurrence sur la dérivation de typage
	    Sauf qu'avec l'env Vide on ne peut pa généralisé alors
	    On généralise avec Eta

	    



----------------------------------------------------
[]	    

	    
	*)

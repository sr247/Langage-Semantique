open Printf
module IS = InstructionSet
              
module Env = Map.Make(String)
type env = value Env.t
 and value =
   | Int of int
   | Closure of string * IS.block * env
   | Unit
   | Addr of int
   (* Equivalent de NULL *)
   | Void

(* Ici, version immuable *)
(*
  type thread_state = {
  code  : block;
  stack : value list;
  env   : env
  }
 *)

type thread_state = {
  id            : int;
  mutable eta   : int;
  mutable code  : IS.block;
  mutable stack : value list;
  mutable env   : env;
}

type machine_state = {
  mutable th : thread_state;
  mutable thl: thread_state Queue.t;
  mutable heap: value array;
  mutable output: string;
  mutable i: int;
  mutable pas: int;
}
                       
(* L'état de la mémoire *)
                       
exception End_of_thread of thread_state
exception End_of_machine of machine_state
exception Not_found_in_Env of string
let debug = true
let th_id = ref 1
let seed = 123456789
                
let step state =
  let fetch() =
    match state.th.code with
    | []   ->
       raise (End_of_thread state.th)
    | i::c ->
       state.th.code <- c;
       i
  in
  let push v =
    state.th.stack <- v::state.th.stack
  in
  let pop() =
    match state.th.stack with
    | [] -> assert false
    | v::s ->
       state.th.stack <- s; v
  in
  (* Ici il s'agit d'une fonction de rezise du tas
     lorsque sa taille devient insuffisante. On double
     la taille *)
  let stretch () =
    let len = Array.length state.heap in
    let rec iter i acc =
      if i >= len then acc
      else
        begin
          acc.(i) <- state.heap.(i);
          iter(i+1) acc
        end
    in
    iter 0 (Array.make (Array.length(state.heap)*2) (Void))
  in
  match fetch() with
  (* Fragment A *)
  | IS.Int(n) ->
     push (Int n)
  | IS.Lookup(id) ->
     let v =
       try
	 Env.find id state.th.env
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
     state.th.env <- (Env.add id v state.th.env)       
  | IS.EndLet(id) ->
     state.th.env <- (Env.remove id state.th.env)
                       
  (* Fragment F *)
  | IS.MkClos(id, c') ->
     let v = Closure(id, c', state.th.env) in
     push(v)
  | IS.Apply ->
     let Closure(id, c', e') = pop() in
     let v = pop() in
     push (Closure("main", state.th.code, state.th.env));
     state.th.code <- c';
     state.th.env <- (Env.add id v e');
  | IS.Return ->
     let v = pop() in
     let Closure(id, c', e') = pop()
     in
     if id = "main" then (* Ici juste par précaution *)
       begin           
	 state.th.stack <- v::state.th.stack;
	 state.th.code <- c';
	 state.th.env <- (Env.remove id e')
       end
     else
       failwith "Not a correct return scheme"

  (* Fragment M *)
  | IS.Unit -> push Unit

  | IS.Alloc ->
     let Addr(i) = state.heap.(0) in
     if (i+1) > 2*Array.length(state.heap)/3 then
       state.heap <- stretch ()
     else
       ();
     state.heap.(0) <- Addr(i+1);
     push (Addr(i))
  | IS.Dup ->
     let v = pop() in
     push v; push v
  | IS.Store ->
     let v = pop() in
     let Addr(i) = pop() in
     state.heap.(i) <- v;
  | IS.Load ->
     let obj = pop () in
     begin
       match obj with
       | Addr(i) -> push (state.heap.(i))
       | _ -> failwith "Not an address"
     end
  |IS.Drop ->
    let garb = pop () in ()
                           
  (* Frangment C *)
  | IS.Spawn ->
     let Closure(id, c', e') = pop() in
     let v = pop () in
     let th = {id=(!th_id); eta=0; code=c'; stack=[Closure("main", [], state.th.env)]; env=(Env.add id v e')} in
     Queue.add state.th state.thl;
     state.th <- th; 
     incr th_id;
     
  (* Extention *)
  | IS.Cond(e1, e2) ->
     let branch =
       match pop() with
       | Int(0) -> e2
       | Int(_) -> e1
       | _ -> failwith "Branch condition has to be Int(n)"
     in
     state.th.code <- branch@state.th.code
  | IS.Loop(c, b) ->
     let loop =
       match pop () with
       | Int(0) -> [IS.Unit]
   
       | Int(_) -> b@[IS.Drop]@c@[IS.Loop(c, b)]
       | _ -> failwith "Loop condition has to be Int(n)"
     in
     state.th.code <- loop @ state.th.code
  | _ -> failwith "Not implemented"

(* **************************************** *)
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

let print_val = function
  | Int(n) -> printf "[ %d ]" n
  | Closure(id, c, _) -> printf"[ "; print_clos id c; printf ") ]"
  | Unit -> printf "[ () ]"
  | Addr(i) -> printf "[ &%d ]" i
  | Void -> printf "[ Void ]"
  | _ -> failwith"Unknown value for print_clos"
                      
let rec print_inst inst =
  let s =
    match inst with
    | IS.Int(n) ->  sprintf " [Int(%d)] " n
    | IS.Lookup(id) -> sprintf " [Lookup(%s)] " id
    | IS.Binop(op) ->
       begin
         match op with
         | IS.Add  -> " [Add] "
         | IS.Mult -> " [Mult] "
         | IS.Sub  -> " [Sub] "
       end
    | IS.Let(id)       -> sprintf " [Let(%s)] " id
    | IS.EndLet(id)    -> sprintf " [EndLet(%s)] " id
    | IS.MkClos(id, c) ->
       sprintf " [MkClos(%s, _)] " id  (* (List.fold_left String.concat "" c) *)
    | IS.Return -> sprintf " [Return] "
    | IS.Apply  -> sprintf " [Apply] "
    | IS.Alloc  -> sprintf " [Alloc] "
    | IS.Store  -> sprintf " [Store] "
    | IS.Load   -> sprintf " [Load] "
    | IS.Dup    -> sprintf " [Dup] "
    | IS.Drop   -> sprintf " [Drop] "
    | IS.Unit   -> sprintf " [Unit] "
    | IS.Spawn  -> sprintf " \x1B[38;5;196m[Spawn]\x1B[0m "
    | IS.Cond(e1, e2) -> sprintf " [Cond(e1, e2)] "
    | IS.Loop(c, e) -> sprintf " [Loop(c, e)] "
    | _ -> failwith"Unknown value for print_inst "
  in printf "%s" s

(* ****************************************** *)

(**
   A fonction which executes the list code "p" retreived from
   the compiling pass and then initiates a "b" state in which:

   b.code = the p list instruction
   b.stack = the stack execution initiate at empty
   b.env = the environment initiate at empty
   
   The function executes instructions by instructions 
   the b.code (using step function) and changes the 
   state of b.stack and the environment.
   Version itérative
 *)
let execute p : unit =
  let t = {id=0; eta=0; code=p; stack=[]; env=Env.empty} in
  let ms = {th=t; thl=Queue.create(); heap=[|Addr(1); Void; Void; Void |]; output=""; i=0; pas=1} in
  let var = 5 in
  let () =
    (* Random.init seed; *)
    Random.self_init ();
    ms.pas <- (Random.int var) + 1
  in
  let exec state =
    while true do
      if debug then
        begin
          printf "\x1B[38;5;63mThread %d Step %d:"
                 state.th.id state.th.eta;
          printf "\n\x1B[0mCode: ";
          List.iter print_inst state.th.code;
          printf "\n\x1B[0m\x1B[38;5;208mStack: ";
          List.iter print_val state.th.stack;
          printf "\n\x1B[38;5;70mHeap[%d]: [| "
                 (Array.length state.heap);
          Array.iter print_val state.heap;
          printf " |]\x1B[0m";
          printf "\n\n";
        end
      else ();
      try
        if Queue.length ms.thl = 0 then
        begin
          step state;
          state.th.eta <- state.th.eta + 1
        end
      else
        begin
          if state.i < state.pas then
            begin
              step state;
              state.th.eta <- state.th.eta + 1;
              state.i <- state.i + 1
            end
          else
            begin
              let current = Queue.take state.thl in
              Queue.add state.th state.thl;
              state.th <- current;
              state.i <- 0;
              state.pas <- (Random.int var) + 1
            end
        end
      with End_of_thread(thi) ->
           printf "\nEnd_of_thread %d\n" thi.id;
           match Queue.is_empty state.thl with
           | true  -> raise (End_of_machine(state))
           | false -> state.th <- Queue.take state.thl
    done
  in
  try
    exec ms
  with End_of_machine(state) -> 
       let () =
         if debug then
           begin
             printf "\nStack: "; List.iter print_val state.th.stack;
             printf "\n";
             printf "Heap[%d]: [| " (Array.length state.heap);
             Array.iter print_val state.heap;
             printf " |]";
             printf "\n";
           end
         else
           ();
         printf"Return: "
       in
       match state.th.stack with
       | Int(n)::s -> printf "%d\n" n
       | Closure(id, c, _)::s -> print_clos id c; printf ")\n"
       | Unit::s -> printf "()\n"
       | Addr(i)::s -> printf "&%d\n" i
       | Void::s -> printf "Void\n"


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
                            *)
                           






    

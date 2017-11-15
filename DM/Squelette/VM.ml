open Options
open Printf
module IS = InstructionSet

(* Code couleur ANSI *)
let default_color = "\x1B[0m"
let thread_color = "\x1B[38;5;45m"
let code_color = "\x1B[38;5;208m"
let heap_color = "\x1B[38;5;177m"
let stack_color = "\x1B[38;5;118m"
let warn_color = "\x1B[38;5;226m"
let error_color = "\x1B[38;5;9m"


(* Variable utilitaires *)
let th_id = ref 1
let seed = 123456789

  
module Env = Map.Make(String)
type env = value Env.t
and value =
  | Int of int
  | Bool of bool
  | Closure of string * IS.block * env
  | Unit
  | Addr of int
   (* Equivalent de NULL *)
  | Void

module Cas = Map.Make(struct type t = int let compare = compare end)
type cas = value Cas.t
                 
type behavior = Wait
		| Join
		| Default

(* Type représentant les threads de la VM *)
type thread_state = {
  id            : int;
  mutable eta   : int;
  mutable code  : IS.block;
  mutable stack : value list;
  mutable env   : env;
  mutable cs    : cas;
}
(* Type représentant la VM elle meme *)
type machine_state = {
  mutable th : thread_state;
  mutable thl: thread_state Queue.t;
  mutable heap: value array;
  mutable output: string;
  mutable i: int;
  mutable pas: int;
  mutable th_policy: behavior;
  mutable rd: int;
}

(* Exception utilitaire *)
exception End_of_thread of thread_state
exception End_of_machine of machine_state
exception Not_found_in_Env of string

                                
let rec string_of_inst inst =
  let s =
    match inst with
    | IS.Int(n) ->  sprintf "[Int(%d)]" n
    | IS.Bool(b) -> sprintf "[Bool(%s)]" (string_of_bool b)
    | IS.Lookup(id) -> sprintf "[Lookup(%s)]" id
    | IS.Binop(op) ->
       begin
         match op with
         | IS.Add  -> "[Add]"
         | IS.Sub  -> "[Sub]"
         | IS.Mult -> "[Mult]"
         | IS.Div  -> "[Div]"
         | IS.Or -> "[Or]"
         | IS.And -> "[And]"
         | IS.Gt -> "[Greater]"
         | IS.Lt -> "[Less]"
         | IS.Ge -> "[Gequal]"
         | IS.Le -> "[Lequal]"
         | IS.Eq -> "[Equal]"
         | IS.Neq -> "[Not]"
         | IS.Eqphy -> "[Phyqual]"
       end
    | IS.Let(id)       -> sprintf " [Let(%s)] " id
    | IS.EndLet(id)    -> sprintf " [EndLet(%s)] " id
    | IS.MkClos(id, c) ->
       sprintf " [MkClos(%s, %s)] " id
               (String.concat "" (List.map (fun i -> string_of_inst i) c))
    | IS.Return -> sprintf " [Return] "
    | IS.Apply  -> sprintf " [Apply] "
    | IS.Alloc  -> sprintf " [Alloc] "
    | IS.Store  -> sprintf " [Store] "
    | IS.Load   -> sprintf " [Load] "
    | IS.Dup    -> sprintf " [Dup] "
    | IS.Drop   -> sprintf " [Drop] "
    | IS.Unit   -> sprintf " [Unit] "
    | IS.Spawn  -> sprintf " %s[Spawn]%s" warn_color code_color
    | IS.Cond(e1, e2) -> sprintf " [Cond(e1, e2)] "
    | IS.Loop(c, e)   -> sprintf " [Loop(c, e)] "
    | IS.Show(len)    -> sprintf " [Show(%d)] " len
    | IS.Wait         -> sprintf "[Wait]"
    | IS.Join         -> sprintf "[Join]"
    | _ -> failwith"VM::string_of_inst::Not implemented"
  in s
       
(**
   Fonction qui permet de mettre en forme les éléments
   pour imprimer sur la sortie output.

   Peut-etre l'intégrer dans un nouveau module spécial print mais .mli à faire etc... peut etre vers la fin du tp
*)        
let val_to_output (n: int) (f:unit-> value) =
  let rec mk_print_list i acc =
    if i = 0 then acc
    else mk_print_list (i-1) (f()::acc)
  in
  let out = (mk_print_list n [])
  in
  String.concat ""
      (List.map
       (fun v ->
         match v with
         | Int(n)  -> sprintf " %d " n
         | Bool(b) -> sprintf " %s " (string_of_bool b)
         | Unit    -> sprintf " () "
         | Addr(a) -> sprintf " &(%d) " a
          (* Peut être on ne devrait pas pouvoir afficher directement une fonction *)
         | Closure(id, c, _) ->
            sprintf "(fun %s -> %s)" id
                    (String.concat "" (List.map (fun i -> string_of_inst i) c))
       ) out)

(** 
    Fonction de cAs pour la gestion de la concurrence
    des threads sur une ou plusieurs variables en 
    mémoire données.
    @param: res 
    Le résultat d'un calcul a stocker dans ladite
    variable.
    @param: i
    La valeur de la case mémoire sur laquelle se fait
    la comparaison de valeur
    @param: state
    L'état de la machine virtuelle dans son ensemble
 *)
let compare_and_swap res i state =
  let sv =
    (* 
       La fonction find_opt fait le mm job
       sauf mais n'existe pas sur les 
       anciennes  version non récent d'Ocaml.
    *)
    try Some(Cas.find i state.th.cs)
    with Not_found -> None
  in
  match sv with
  | None -> state.th.cs <- Cas.add i res state.th.cs;
            state.heap.(i) <- res;
            true
  | Some(v) ->
     if v = state.heap.(i) then
       begin
         state.heap.(i) <- res;
         true
       end
     else
       begin
	 (* printf "%sLa valeur a changé donc swap%s\n" error_color default_color; *)
	 state.th.code <- [IS.Load]@[IS.Drop]@[IS.Store]@state.th.code;
	 state.th.stack <- [Addr(i)]@[v]@[Addr(i)]@state.th.stack;
         false
       end

let delay state =    
  Queue.add state.th state.thl;
  state.th <- Queue.take state.thl;
  state.i <- 0;
  state.pas <- (Random.int state.rd) + 1
                                   
let step state =
  (* Mécanisme de Compare and Swap 
     D'abors on save la valeur load dans un env spécial
     (cs) avec retains *)
  let retains i v =
    state.th.cs <- Cas.add i v state.th.cs
  in
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
     lorsque sa taille devient insuffisante; on double
     la taille. Et on réécrit les précédentes valeurs
     aux meme adresses *)
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
  | IS.Bool(b) ->
     push (Bool(b))
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
       | IS.Div ->
	  let Int n1 = pop() in
	  let Int n2 = pop() in
	  push(Int(n1/n2))
       | IS.Eq ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1=v2))
       | IS.Neq ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1<>v2))
       | IS.Eqphy ->
          let Addr(i) = pop() in
          let Addr(j) = pop() in
          push(Bool(i==j))
       | IS.Gt ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1>v2))
       | IS.Ge ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1>=v2))
       | IS.Lt ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1<v2))
       | IS.Le ->
          let v1 = pop() in
          let v2 = pop() in
          push(Bool(v1<=v2))
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
  (* Ici petite optimisation supplémentaire:
     si l'indice du pointeur sur la dernière cas alloué 
     est supérieur à une certain fraction de la taille 
     du tas, alors on "étire" le tas.
  *)
  | IS.Alloc ->
     let Addr(i) = state.heap.(0) in
     if (i+1) > 9*Array.length(state.heap)/10 then
       state.heap <- stretch ()
     else ();
     state.heap.(0) <- Addr(i+1);
     push (Addr(i))
  | IS.Dup ->
     let v = pop() in
     push v; push v
  | IS.Store ->
     let v = pop() in
     let Addr(i) = pop() in
     begin
       match compare_and_swap v i state with
       | true -> ()
       | false -> delay state 
     end
  | IS.Load ->
     let obj = pop () in
     begin
       match obj with
       | Addr(i) ->
          let v = state.heap.(i) in
          push (v); retains i v
       | _ -> failwith "Not an address"
     end
  |IS.Drop -> ignore (pop ())
     
  (* Frangment C *)
  | IS.Spawn ->
     let Closure(id, c', e') = pop() in
     let v = pop () in
     let th = {id=(!th_id); eta=0; code=c'; stack=[Closure("main", [], state.th.env)]; env=(Env.add id v e'); cs=Cas.empty} in
     Queue.add th state.thl;
     incr th_id;
     
  (* Extention *)
  | IS.Cond(e1, e2) ->
     let branch =
       match pop() with
       | Int(0) -> e2
       | Int(_) -> e1
       | _ -> failwith "Branch condition has to be Int(n)" (* Replace with Bool(b) *)
     in
     state.th.code <- branch@state.th.code
  | IS.Loop(c, b) ->
     let loop =
       match pop () with
       | Int(0) -> [IS.Unit]
       | Int(_) -> b@[IS.Drop]@c@[IS.Loop(c, b)]
       | _ -> failwith "Loop condition has to be Int(n)"  (* Replace with Bool(b) *)
     in
     state.th.code <- loop @ state.th.code
  | IS.Show(n) ->
     state.output <- val_to_output n pop;
    printf "%s\n" state.output
  | IS.Wait -> state.th_policy <- Wait
  | IS.Join -> state.th_policy <- Join
  | _ -> failwith "VM::step::Not implemented"

(* **************************************** *)
let rec string_of_clos id c =
  let s =
    (String.concat "" (List.map (fun i -> string_of_inst i) c))
  in sprintf "( fun %s -> %s)" id s 
  

(* Ici repose toutes les fonctions d'affichages
   pour le débug.
*)
and string_of_val v =
  let s =
    match v with
    | Int(n) -> sprintf "[ %d ]" n
    | Bool(b) -> sprintf "[ %s ]" (string_of_bool b)
    | Closure(id, c, _) -> sprintf"[ (%s) ]" (string_of_clos id c)
    | Unit -> sprintf "[ () ]"
    | Addr(i) -> sprintf "[ &%d ]" i
    | Void -> sprintf "[ Void ]"
    | _ -> failwith"VM::print_val::Unknown value"
  in s
       


(* *************************************************************************** *)

let print_'a stringify x =
  printf "%s" (stringify x) 
       
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
let execute (p: IS.t list) : unit =
  let t = {id=0; eta=0;
           code=p; stack=[]; env=Env.empty;
           cs=Cas.empty} in
  let ms = {th=t;
            thl=Queue.create();
            heap=[|Addr(1); Void; Void; Void |];
            output=""; i=0;
            pas=1;
            th_policy=Default;
            rd=5} in
  let () =
    (* Random.init seed; *)
    Random.self_init ();
    ms.pas <- (Random.int ms.rd) + 1
  in
  let exec state =
    while true do
      (* Partie Debug *)
      if !debug then
        begin
          printf "---------------------------------------------------------------";

          printf "\n%sThread %d Step %d:"
                 thread_color state.th.id state.th.eta;

          if !showCode then printf "\n%sCode: " code_color;
          List.iter (print_'a string_of_inst) state.th.code;
          if !showStack then printf "\n%s%sStack: "
            default_color stack_color;
          List.iter (print_'a string_of_val) state.th.stack;
          if !showHeap then
            begin
              printf "\n%sHeap[%d]: [| " heap_color
                     (Array.length state.heap);
              Array.iter (print_'a string_of_val) state.heap;
              printf " |]%s" default_color;
            end
          else ();
          printf "\n---------------------------------------------------------------\n";
          printf "\n";
        end
          (* Partie Debug *)
      else ();
      try
        if Queue.length ms.thl = 0 then
          begin
            step state;
            state.th.eta <- state.th.eta + 1
          end
	else
          begin
            if state.i >= state.pas
            then delay state
            else ();
	    step state;
	    state.th.eta <- state.th.eta + 1;
	    state.i <- state.i + 1
          end
      with End_of_thread(thi) ->
        (* printf "\nEnd_of_thread %d\n" thi.id; *)
        match state.th_policy with
        | Wait ->
           if thi.id = 0 then 
             begin
               match Queue.is_empty state.thl with
               | true  -> raise (End_of_machine(state))
               | false ->
                  Queue.add thi state.thl;
                  state.th <- Queue.take state.thl
             end
        | Join -> ()
        | Default ->
           begin
             match Queue.is_empty state.thl with
             | true  -> raise (End_of_machine(state))
             | false -> state.th <- Queue.take state.thl
           end
    done
  in
  try
    exec ms
  with End_of_machine(state) -> 
    let () =
      if !debug then
        begin
          printf "\nStack: "; List.iter (print_'a string_of_val) state.th.stack;
          printf "\n";
          printf "Heap[%d]: [| " (Array.length state.heap);
          Array.iter (print_'a string_of_val) state.heap;
          printf " |]";
          printf "\n";
        end
      else
        ();
      if !debug then
        printf"Return: "
      else ();
    in
    match state.th.stack with
    | Int(n)::s -> printf "%d\n" n
    | Bool(b)::s -> printf "%s\n" (string_of_bool b)
    | Closure(id, c, _)::s -> printf "%s\n" (string_of_clos id c)
    | Unit::s -> printf "\n"
    | Addr(i)::s -> printf "&%d\n" i
    | Void::s -> printf "Void\n"
    | _ -> failwith "VM::execute::Not implemented"


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
       

       

(* Fonction réserve *)

(* Rétrécie le tas -> sorte de GC*)
(* let shrink () = *)
(*   let len = Array.length state.heap in *)
(*   let rec iter i acc = *)
(*     if i >= len then acc *)
(*     else *)
(*       begin *)
(*         acc.(i) <- state.heap.(i); *)
(*         iter(i+1) acc *)
(*       end *)
(*   in *)
(*   iter 0 (Array.make (Array.length(state.heap)/2 + 1) (Void)) *)
(*     in *)

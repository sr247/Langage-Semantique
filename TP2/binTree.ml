open Printf

exception Element_Found
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(*
  Créer une liste des noeuds de l'arbre dan l'ordre d'un parcours en profondeur.
*)

let rec dfs t =
  match t with
  | Leaf -> [] 
  | Node (a, t, u) -> a :: (dfs t) @ (dfs u)

(* 
Question1 : Réécrire [dfs] en style par passage de continuations.
Le résultats doit etre récursif terminal.
Question2: écrire en CPS une fonction [préfix: a -> a tree -> a list]
qui affiche les éléments rencontrés avant le premier paramètre. 
Utiliser deux continuations pour distinguer le cas ou l'élément a été trouvé ou non.
Question3 :  écrire en CPS une fonction [find : a -> a tree -> a list options] telle que, si [a] existe dans [t] alors [find a t] renvoie la liste des noeuds sur le chemin de la racine vers [a]
*)

let atree = Node (2, Node (5, Leaf, Leaf), Node (3, Leaf, Leaf))
(* Question 1 : renvoyer la liste des noeuds en parcours dfs *)
let cps_dfs_test (t: int tree) =
  let rec dfs_aux (t: 'a tree) (k: 'a list -> 'a list) : 'a list =
    match t with
    | Leaf -> k []
    | Node(a, t, u) -> dfs_aux t (fun dfst -> dfs_aux u (fun dfsu -> k (a :: dfst @ dfsu)) )
  in
  dfs_aux t (fun x -> x)

(* Question 1 : afficher la liste des noeuds en dfs *)
let cps_dfs (t: int tree) : unit =
  let rec dfs_aux (t: 'a tree) (k: 'a list -> 'b)  : 'b=
    match t with
    | Leaf -> k []
    | Node(a, t, u) -> dfs_aux t (fun dfst -> dfs_aux u (fun dfsu -> k (a :: dfst @ dfsu)) )
  in
  dfs_aux t (List.iter print_int)


(* Question 2 : *)
(* Ici jouer avec des listes n'est peut etre pas la meilleure solution *)
(* Envisager de jouer peut-etre directement avec les trees *)
let cps_print_before (e: int) (t: int tree) =
  let rec cps_print_aux (t: 'a tree) (k1: 'a -> unit) (k2: 'a -> unit) =
    match t with
    | Leaf -> k1 0 (* Printf.printf "Missed " *)
    | Node(a, t, u) -> (* Printf.printf "A value %d, " a; *)
       cps_print_aux t (fun prntT -> cps_print_aux u (fun prntU -> if e <> a then k1 a else k2 a) (fun x -> ())) (fun noExectpion -> ())
 in
 cps_print_aux t print_int (fun x -> raise Element_Found)



(* Deux continuation ici : soit on est différent de a alors on print
Des qu'uon est égal à a : on fait la continuation qui try with : StopIteration *)
(* cps_print_before 5 Node(2, Node(5, Leaf, Leaf), Node(3, Leaf, Leaf)) *)
(* Test *)

(* let btree = Node("2", Node("6", Leaf, Leaf), Node("3", Node("8", Leaf, Node("4", Leaf, Leaf)), Leaf) ) *)
(* let () = cps_print_before_One_k "5" btree *)


                                
(* Question 3 : indication *)
exception Found of int list
                       
let find a t =
  let rec find t = match t with
    | Leaf -> ()
    | Node(b, g, d) ->
       if a = b
       then raise (Found [])
       else
	 try find g; find d
	 with Found(path) -> raise (Found(b::path))
  in
  try find t; None
  with Found(path) -> Some path
                           
(* let double_barrel_cps_find (a:string) (t:string tree) = *)
(*   let rec find (t:'a tree) (kf:'a -> unit) (knf:'a -> unit) = *)
let double_barrel_cps_find a t =
  let rec find t kf knf =
    match t with
    | Leaf -> printf "Leaf\n"; knf ()
    | Node(e, g, d) ->
       if e = a then
         begin printf "Node %d\n" e; kf e end
       else
         begin
           try  printf "Node %d\n" e; find g kf knf; find d kf knf
           with Found(pathlist) -> raise (Found (e::pathlist))
         end
  in
  try
    find t (fun x -> raise (Found [x])) (fun y -> y); None
  with Found(path) -> Some path
                                      
(* let ctree = Node (2, Node (5, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf))) *)
(* let etree = Node (3, Node(4, Leaf, Leaf), Leaf)                              *)
let print_path a t =
  let res = double_barrel_cps_find a t in
  match res with
  |None -> printf "\nNo path found\n"
  |Some(a) -> printf "\n[ " ;
              List.iter (fun x -> print_int x; printf " ") a;
              printf "]\n"

                     
let buildtree n =
  let () = Random.self_init () in
  let rec aux acc n =
    if n > 0 then Node(Random.int n, aux acc (n-1), aux acc (n-1))
    else acc
  in aux Leaf n
    
let e = buildtree 24

let _ = print_path 4 e

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

     
(* Question 1 : *)
let cps_dfs (t: int tree) : unit =
  let rec dfs_aux (t: 'a tree) (k: 'a list -> 'b) =
    match t with
    | Leaf -> k []
    | Node(a, t, u) -> dfs_aux t (fun dfst -> dfs_aux u (fun dfsu -> k (a :: dfst @ dfsu)) )
  in
  dfs_aux t (List.iter print_int)


(* Question 2 : *)
(* Ici jouer avec des listes n'est peut etre pas la meilleure solution *)
(* Envisager de jouer peut-etre directement avec les trees *)
let cps_print_before (e: int) (t: int tree) : unit =
  let rec cps_print_aux (e': int) (t: int tree) (k1: 'a list -> unit) (k2: 'a list -> unit) =
  match t with
  | Leaf -> ()
  | Node(a, s, u) -> cps_print_aux e' (fun prntL -> k1  
  (* | Node(a, t, u) -> if e = a then print_string a; cps_print_aux e (fun  *)
(* Deux continuation ici : soit on est différent de a alors on print
Des qu'uon est égal à a : on fait la continuation qui try with : StopIteration *)
     
(* Question 3 : indication    *)       
(* let double_barrel_cps_find a t = *)
(*   let rec find t kf knf = *)
(*     match t with *)
(*     | Leaf -> knf [] *)
(*     | Node(e, t, u) -> *)
(*        if e = a then find t (fun fnd -> *)
(*          cps (evaluateur lambda) *)

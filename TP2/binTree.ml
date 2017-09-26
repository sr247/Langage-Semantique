type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

  (*
    Créer une liste des noeuds de l'arbre dan l'ordre d'un parcours en profondeur.
*)

let rec dfs t =
  match t with
  | Leaf -> [] 
  | Node (a, t, u) -> f (a:: (dfs t) @ (dfs u))

(* 
Question1 : Réécrire [dfs] en style par passage de continuations.
Le résultats doit etre récursif terminal.
Question2: écrire en CPS une fonction [préfix: a -> a tree -> a list]
qui affiche les éléments rencontrés avant le premier para mètre. utiliser deux continuation pour distinguer le cas ou l'élément a été trouvé ou non.
Question3 :  écrire en CPS une fonction [find : a -> a tree -> a list options] telle que, si [a] existe dans [t] alors [find a t] renvoie la liste des noeuds sur le chemin de la racine vers [a]
*)


Q1
let cps_dfs (t: int tree) : unit =
  let rec dfs_aux (t: 'a tree) (k: 'a list -> 'b) =
    match t with
    | Leaf -> k []
    | Node(a, t, u) -> dfs_aux t (fun dfst -> dfs_aux u (fun dfsu -> k (a :: dfst @ dfsu)) )
  in
  dfs_aux t (List.iter print_int)

     
    Q3 indication
let double_barrel_cps_find a t =
  let rec find t kf knf =
    match t with
    | Leaf -> knf []
    | Node(e, t, u) ->
       if e = a then find t (fun fnd ->



        cps (evaluateur lambda)

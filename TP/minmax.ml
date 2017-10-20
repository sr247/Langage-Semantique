(**
   Référence biblio :
   'Why functional programming matters' (John Hughes)

   Lien vers le fichier :
   http://www.lri.fr/~blsk/minmax.ml

   Cet exercice est organisé autour de l'algorithme MinMax,
   qui a pour but de décider d'un coup à jouer dans un jeu
   à deux joueurs.

   Idée principale de l'algorithme : explorer l'arbre des [k] coups
   suivants possibles, et attribuer une valeur à chaque position du
   jeu explorée de la manière suivante :

   - les feuilles de l'arbre, c'est-à-dire les positions limites
     de l'exploration, ont une valeur donnée par une estimation grossière
     de la qualité de la position elle-même (par exemple : la différence
     entre les scores du joueur et de l'adversaire,

   - les positions internes de l'arbres ont une valeur calculée à partir
     des valeurs des positions suivantes, en supposant que le joueur et
     l'adversaire jouent 'bien' : lors d'un coup du joueur on garde la
     valeur maximisant son gain, et lors d'un coup de l'adversaire, on
     garde la valeur minimisant le gain du joueur (parmi les valeurs
     préalablement estimées pour les positions suivantes).

   On propose d'abord un code dit 'spaghetti', incluant toute l'exploration
   dans une unique fonction, puis un code plus modulaire mais inefficace à
   base de fonctions très simples. L'objectif de l'exercice est d'ajouter
   de l'évaluation paresseuse à cette deuxième version modulaire, afin de la
   rendre efficace.
*)

(* Signature décrivant un jeu. *)
module type GameSig = sig
  (* Le type [position] désigne une configuration du jeu. *)
  type position
  (* Étant donnée une position du jeu la fonction [moves] donne la liste
     des coups possibles, sous la forme des nouvelles positions résultant de
     ces coups. *)
  val moves: position -> position list
  (* La fonction [eval] donne une évaluation grossière de la qualité d'une
     position donnée, du point de vue du joueur. *)
  val eval : position -> int
end

(* Un module instanciant GameSig avec le jeu de Nim. *)
module Nim : GameSig = struct
  (* Position du jeu : nombre d'allumettes présentes, et booléen valant
     [true] si c'est au tour du joueur de choisir le prochain coup. *)
  type position = int * bool
  (* Jouer un coup consiste à retirer 1, 2 ou 3 allumettes. *)
  let moves (p, b) =
    let nb = not b in
    if p>=3
    then [ p-1,nb ; p-2,nb ; p-3,nb ]
    else if p=2
    then [ 0,nb ; 1,nb ]
    else if p=1
    then [ 0,nb ]
    else []
  (* Une position est gagnante pour le joueur si
     - l'adversaire est le prochain à jouer et les allumettes sont en
       nombre divisible par 4
     - le joueur est le prochain à jouer et les allumettes sont en
       nombre non divisible par 4
  *)
  let eval (p, b) =
    if b
    then if p mod 4 = 0 then 0 else 1
    else if p mod 4 = 0 then 1 else 0
end

module ListExtraOps = struct
  (* Équivalent des fonctions [fold] habituelles, mais suppose que
     la liste est non vide et prend le premier élément comme valeur
     initiale de l'accumulateur. *)
  let lfold f l =
    let rec fold f acc = function
      | []   -> acc
      | a::l -> fold f (f acc a) l
    in
    match l with
      | []   -> failwith "Empty list"
      | a::l -> fold f a l

  (* Renvoie le maximum/minimum d'une liste non vide. *)
  let lmax = lfold max
  let lmin = lfold min
end

(* Première version de l'exploration. *)
module SpaghettiMinMax (G : GameSig) = struct
  open ListExtraOps
    
  (* [evaluate_max k p] calcule la valeur de la position [p] en
     explorant les [k] prochains coups possibles.
     Cette fonction suppose que c'est au tour du joueur de choisir un coup.
  *)
  let rec evaluate_max k p =
    (* Si [k] est nul, utiliser la fonction d'évaluation grossière. *)
    if k <= 0 then G.eval p
    else
      let next_list = G.moves p in
      if next_list = []
      (* S'il n'y a aucun coup à jouer à partir de [p], utiliser également
	 le fonction d'évaluation grossière. *)
      then G.eval p
      (* Évalue les valeurs des positions suivantes, en explorant [k-1] coups
	 et en utilisant la fonction [evaluate_min] qui suppose que c'est à
	 l'adversaire de jouer. Puis prendre le maximum de cette liste. *)
      else let next_values = List.map (evaluate_min (k-1)) next_list in
	   ListExtraOps.lmax next_values

  and evaluate_min k p =
    (* Même chose, mais du point de vue de l'adversaire, donc en inversant
       les recours à [min] et [max]. *)
    if k <= 0 then G.eval p
    else 
      let next_list = G.moves p in
      if next_list = []
      then G.eval p
      (* Au passage, observez l'utilisation de l'opérateur de composition |>
	 qui prend le résultat de l'expression de gauche et le donne en
	 argument à l'expression de droite. *)
      else List.map (evaluate_max (k-1)) next_list |> lmin

  (* Par défaut, on considère que c'est le tour du joueur. *)
  let evaluate k p = evaluate_max k p
	  
end


(* Une version plus modulaire, où chaque étape de l'exploration est
   bien délimitée.  *)
module MinMax (G : GameSig) = struct
  (* Nous allons découper le travail en quatre étapes :
     1. Construire l'arbre des coups possibles
     2. Couper à profondeur [k]
     3. Affecter à chaque position sa valeur grossière
     4. Calculer les maximisations/minisations
  *)

    
  (* On donne déjà un type à l'arbre des coups possibles.
     Un nœud est formé par la position courante et une liste d'arbre
     correspondant aux coups suivants. *)
  type 'a tree = Node of 'a * ('a tree list)

  (* Étape 1 : Construire l'arbre des coups possibles
       mk_tree: ('a -> 'a list) -> 'a -> 'a tree
     Cette fonction construit l'arbre à partir d'une fonction [next] qui
     calcule les fils d'une position [p] donnée.
     Le principe sera d'instancier [next] par [G.moves].
  *)
  let rec mk_tree next p =
    Node(p, List.map (mk_tree next) (next p))

  (* Étape 2 : Couper à profondeur [k]
       prune: int -> 'a tree -> 'a tree
  *)
  let rec prune k = function
    | Node(p, next_ps) ->
      if k = 0
      then Node(p, [])
      else Node(p, List.map (prune (k-1)) next_ps)

  (* Étape 3 : Affecter à chaque position sa valeur grossière
       map_tree: ('a -> 'b) -> 'a tree -> 'b tree
     La fonction [map_tree] construit un nouvel arbre dont les étiquettes
     sont données par une fonction [f].
     Le principe sera d'instancer [f] par [G.eval].
  *)
  let rec map_tree f = function
    | Node(a, ts) -> Node(f a, List.map (map_tree f) ts)

  (* Étape 4 : Calculer les maximisations/minisations
       maximize: int tree -> int
       minimize: int tree -> int
  *)
  let rec maximize = function
    | Node(n, []) -> n
    | Node(n, l)  -> ListExtraOps.lmax (List.map minimize l)
  and minimize = function
    | Node(n, []) -> n
    | Node(n, l)  -> ListExtraOps.lmin (List.map maximize l)

  (* Fonction finale [evaluate] : appelle les quatre fonctions précédentes
     dans l'ordre, en instanciant les fonctions spécifiques au jeu par
     [G.moves] et [G.eval] *)
  let evaluate k p =
    mk_tree G.moves p |> prune k |> map_tree G.eval |> maximize

(* Note.
   L'enchaînement de compositions |> est équivalent au code suivant :
   let tree  = mk_tree G.moves p in
   let ptree = prune k tree in
   let etree = map_tree G.eval ptree in
   maximize etree

   On pourrait également l'écrire comme cela :
   maximize (map_tree G.eval (prune k (mk_tree G.moves p)))
*)	

end

  
module Lazy = struct
  (* Ce qu'on veut représenter : un calcul suspendu, qu'on n'effectue pas
     tout de suite, et dont on mémorisera le résultat une fois qu'il sera
     connu.
  *)

  (* On définit d'abord un type décrivant les deux états possibles :
     déjà évalué ou pas encore. *)
  type 'a t_aux =
    | Valeur   of 'a
    | Suspendu of (unit -> 'a)
  (* On considère à la fin une référence sur l'état précédent, afin de
     pouvoir mettre à jour lors de l'évaluation. *)
  type 'a t = 'a t_aux ref

  (* Évaluer la suspension (et changer son état pour qu'elle soit ensuite
     connue comme déjà évaluée)
       force : 'a t -> 'a
  *)
  let force e =
    match !e with
      | Suspendu(f) -> let v = f() in  (* Calcul de la valeur   *)
		       e := Valeur(v); (* Mise à jour de l'état *)
		       v               (* Renvoi de la valeur   *)
      | Valeur(v)   -> v  (* Renvoi immédiat de la valeur déjà connue *)

  (* Construire une suspension *)
  let mk_lazy f = ref (Suspendu f)
      

end
  
  
module LazyMinMax (G : GameSig) = struct

  (* On modifie le type de l'arbre des coups possibles pour le rendre
     paresseux.
     Maintenant un arbre est une suspension, qui renvoie un nœud une fois
     forcée. Remarque : les fils d'un nœud sont à nouveau des suspensions
     qui devront être forcées à leur tour. Les nœuds seront ainsi découverts
     l'un après l'autre, chacun uniquement lorsqu'il sera nécessaire.
  *)
  type 'a tree = 'a tree_node Lazy.t
  and  'a tree_node = Node of 'a * ('a tree list)
    
  (* mk_tree: ('a -> 'a list) -> 'a -> 'a tree *)
  let rec mk_tree next p =
    Lazy.mk_lazy(fun () ->
      Node(p, List.map (mk_tree next) (next p))
    )

  (* map_tree: ('a -> 'b) -> 'a tree -> 'b tree *)
  let rec map_tree f t =
    match Lazy.force t with
      | Node(p, next_ps) ->
	Lazy.mk_lazy(fun () ->
	  Node(f p, List.map (map_tree f) next_ps)
	)

  (* maximize: int tree -> int *)
  (* minimize: int tree -> int *)
  let rec maximize = function
    | Node(n, []) -> n
    | Node(n, l)  -> ListExtraOps.lmax (List.map minimize l)
  and minimize = function
    | Node(n, []) -> n
    | Node(n, l)  -> ListExtraOps.lmin (List.map maximize l)


  (* prune: int -> 'a tree -> 'a tree *)
  let rec prune k = function
    | Node(a, ts) ->
      if k = 0
      then Node(a, [])
      else Node(a, List.map (prune (k-1)) ts)

  let evaluate k p =
    mk_tree G.moves p |> prune k |> map_tree G.eval |> maximize
	
end

(**
   Suggestion une fois que vous aurez fini : lisez l'article mentionné
   en introduction, et suivez l'approche proposée pour passer du simple
   algorithme MinMax à l'algorithme amélioré AlphaBeta.
*)
  
module SpaghettiTest = SpaghettiMinMax(MB)
let _ = print_int (SpaghettiTest.evaluate 15 1000290)

module GameTest = MinMax(Nim)
let _ = print_int (GameTest.evaluate 3 21)
  

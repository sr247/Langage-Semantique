(**
   RÃ©fÃ©rence biblio :
   'Why functional programming matters' (John Hughes)
*)

module type GameSig = sig
  type position
  val  moves: position -> position list
  val  eval : position -> int
  val  to_string : position -> string
end

(* *)
module MB = struct
  type position = int
  let moves p =
    if p>=3
    then [ p-1; p-2; p-3 ]
    else if p=2
    then [0; 1]
    else if p=1
    then [0]
    else []
  let eval p =
    if p mod 4 = 0 then 1 else 0
  let to_string = string_of_int
end

let lfold f l =
  let rec fold f acc = function
    | []   -> acc
    | a::l -> fold f (f acc a) l
  in
  match l with
    | []   -> failwith "Empty list"
    | a::l -> fold f a l
      
let lmax = lfold max
let lmin = lfold min

module SpaghettiMinMax (Game : GameSig) = struct
  module G = Game

  let rec evaluate_max k p =
    if k <= 0 then G.eval p
    else 
      let next_list = G.moves p in
      if next_list = []
      then G.eval p
      else List.map (evaluate_min (k-1)) next_list |> lmax
  and evaluate_min k p =
    if k <= 0 then G.eval p
    else 
      let next_list = G.moves p in
      if next_list = []
      then G.eval p
      else List.map (evaluate_max (k-1)) next_list |> lmin

  let evaluate k p = evaluate_max k p
	  
end


module Lazy = struct
	(* Ce qu'on veut représenter : un calcul suspendu,
	qu'on effectue pas tout de suite, et on mémorisera le résultat une fois qu'il sera connu
	*)
	type 'a t = 'a t_aux ref and 'a t_aux = 
	|Suspendu of (unit -> 'a)
	|Valeur of 'a
	
	let mk_lazy : (unit -> 'a) -> 'a t = fun f ->
	ref(Suspendu f)  (* Construire une suspension *)
	(* Evaluer la suspension *)
	let force : 'a t -> 'a = fun e ->
	match !e with
	| Suspendu(f)-> let v = f() in      (* evaluation*)
						e:= Valeur(v);
						v
	| Valeur(v)-> v
	
end

  
module MinMax (Game : GameSig) = struct

  module G = Game
  type 'a tree = 'a tree_node Lazy.t
  and 'a tree_node = Node of 'a * ('a tree list)
	
  (* Type Actuel:  ('a -> 'a list) -> 'a tree *)
  (* Type Souhaité:  ('a -> 'a list) -> 'a -> 'a tree Lazy.t *)
  let rec mk_tree f a = Lazy.mk_lazy(fun () -> Node(a, List.map (mk_tree f) (f a)))

  (* Type : 
	 ('a -> 'b) -> 'a tree -> 'b tree
  *)
  let rec map_tree f t = 
  match Lazy.force t with
    | Node(a, ts) -> Lazy.make_lazy(Node(f a, List.map (map_tree f) ts))
	
	(* Type :
		int tree -> int
	*)
  let rec maximize = function
    | Node(n, []) -> n
    | Node(n, l)  -> lmax (List.map minimize l)
  and minimize = function
    | Node(n, []) -> n
    | Node(n, l)  -> lmin (List.map maximize l)

	(* Type :
		(int -> 'a tree -> 'a tree)
	*)
	
  let rec prune k = function
    | Node(a, ts) ->
      if k = 0
      then Node(a, [])
      else Node(a, List.map (prune (k-1)) ts)

  let evaluate k p =
    mk_tree G.moves p
    |> prune k
    |> map_tree G.eval
    |> maximize 
    
    (**
		let evaluate k p =
		let tree = mk_tree G.moves p in
		let ptree = prune k tree in
		let 
    *)
	
end

module GameTest = MinMax(MB)
	let _ = print_int(GameTest.evaluate 3 24)
	

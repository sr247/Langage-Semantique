open Printf

exception Found of int list

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(* En se basant sur la fonction que vous avez donné *)
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


(* L'implementation de ma fonction *)
(**
   param:  a: int
   t: int tree
   return: int list option

*)
let double_barrel_cps_find_incon a t =
  let rec find t kf knf =
    match t with
    | Leaf -> printf "Leaf\n"; knf a
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
    find t (fun x -> raise (Found [x])) (fun y -> ()); None
  with Found(path) -> Some path

(* Un continuation peut en avoir une autre en paramètre ? Si oui alors knf *)
let double_barrel_cps_find a t =
  let rec find t kf knf =
    match t with
    | Leaf -> printf "Leaf\n"; knf ()
    | Node(e, g, d) -> printf "Node %d\n" e;
      kf e (fun () -> find g kf knf; find d kf knf)
  in
  find t (fun x knf ->
    if x = a then raise (Found [x])
    else try knf () with Found(pathlist)-> raise (Found(x::pathlist)) ) 
    (fun y -> ())

    
let ctree = Node (2, Node (5, Leaf, Leaf), Node(3, Leaf, Node(4, Leaf, Leaf)))
let etree = Node (3, Node(4, Leaf, Leaf), Leaf)                             



let print_path a t =
  let res = double_barrel_cps_find_incon a t in
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

let _ = double_barrel_cps_find 4 etree

type t = E | T of t * int * int * t

let rec insert (k : int) (v : int) (t : t) =
  match t with
  | E -> T (E, k, v, E)
  | T (l, k', v', r) ->
      (*! insert *)
      if k < k' then T (insert k v l, k', v', r)
      else if k' < k then T (l, k', v', insert k v r)
      else T (l, k', v, r)
      (*!! insert_1 *)
      (*!
      let _ = ignore (l, k', v', r, insert) in
      T (E, k, v, E)
      *)
      (*!! insert_2 *)
      (*!
      if k < k' then T ((insert k v l), k', v', r)
      else T (l, k', v, r)
      *)
      (*!! insert_3 *)
      (*!
      if k < k' then T ((insert k v l), k', v', r)
      else if k' < k then T (l, k', v', (insert k v r))
      else T (l, k', v', r)
      *)
      (* !*)

let rec join (l : t) (r : t) =
  match (l, r) with
  | E, _ -> r
  | _, E -> l
  | T (l, k, v, r), T (l', k', v', r') -> T (l, k, v, T (join r l', k', v', r'))

let rec delete (k : int) (t : t) =
  match t with
  | E -> E
  | T (l, k', v', r) ->
      (*! delete *)
      if k < k' then T (delete k l, k', v', r)
      else if k' < k then T (l, k', v', delete k r)
      else join l r
      (*!! delete_4 *)
      (*!
  let _ = ignore v' in
  if k < k' then delete k l
  else if k' < k then delete k r
  else join l r
      *)
      (*!! delete_5 *)
      (*!
  if k' < k then T ((delete k l), k', v', r)
  else if k < k' then T (l, k', v', (delete k r))
  else join l r
      *)
      (* !*)

let rec below (k : int) (t : t) =
  match (k, t) with
  | _, E -> E
  | k, T (l, k', v, r) -> if k <= k' then below k l else T (l, k', v, below k r)

let rec above (k : int) (t : t) =
  match (k, t) with
  | _, E -> E
  | k, T (l, k', v, r) -> if k' <= k then above k r else T (above k l, k', v, r)

let rec union (l : t) (r : t) =
  match (l, r) with
  | E, _ -> r
  | _, E -> l
  (*! union *)
  | T (l, k, v, r), t -> T (union l (below k t), k, v, union r (above k t))
  (*!! union_6 *)
  (*!
  | T (l, k, v, r), T (l', k', v', r') -> T (l, k, v, T (union r l', k', v', r'))
  *)
  (*!! union_7 *)
  (*!
  | T (l, k, v, r), T (l', k', v', r') ->
      if k == k' then T (union l l', k, v, union r r')
      else if k < k' then T (l, k, v, T (union r l', k', v', r'))
      else union (T (l', k', v', r')) (T (l, k, v, r))
  *)
  (*!! union_8 *)
  (*!
  | T (l, k, v, r), T (l', k', v', r') ->
      if k == k' then T (union l l', k, v, union r r')
      else if k < k' then
        T (union l (below k l'), k, v, union r (T (above k l', k', v', r')))
      else union (T (l', k', v', r')) (T (l, k, v, r))
  *)
  (* !*)

let rec find (k : int) (t : t) : int option =
  match (k, t) with
  | _, E -> None
  | k, T (l, k', v', r) ->
      if k < k' then find k l else if k' < k then find k r else Some v'

let rec size (t : t) =
  match t with E -> 0 | T (l, _, _, r) -> 1 + size l + size r

(** Helper functions *)

let rec keys (t : t) : int list =
  match t with
  | E -> []
  | T (l, k, _v, r) ->
      let lk = keys l in
      let rk = keys r in
      [ k ] @ lk @ rk

let rec is_bst (t : t) : bool =
  let open List in
  match t with
  | E -> true
  | T (l, k, _, r) ->
      is_bst l && is_bst r
      && for_all (fun k' -> k' < k) (keys l)
      && for_all (fun k' -> k' > k) (keys r)

(** Removes from a key-value list *)
let delete_key k kvs = List.filter (fun (k', _) -> k <> k') kvs

(** Insert into sorted key-value list. Replaces value if the key exists *)
let rec l_insert k v kvs =
  match kvs with
  | [] -> [ (k, v) ]
  | (k', v') :: kvs' ->
      if k < k' then (k, v) :: kvs
      else if k = k' then (k, v) :: kvs'
      else (k', v') :: l_insert k v kvs'

let rec is_sorted l =
  match l with
  | [] -> true
  | (k, _) :: l' -> (
      match l' with [] -> true | (k', _) :: _l'' -> k < k' && is_sorted l')

let rec to_list t =
  match t with E -> [] | T (l, k, v, r) -> to_list l @ [ (k, v) ] @ to_list r

let rec l_union l1 l2 =
  match l1 with
  | [] -> l2
  | (k, v) :: l1' ->
      let l2' = List.filter (fun (k', _) -> k <> k') l2 in
      (k, v) :: l_union l1' l2'

let eq t1 t2 = to_list t1 = to_list t2
let ( === ) t1 t2 = eq t1 t2

(** Others *)
let to_string t =
  let rec aux t =
    match t with
    | E -> "(E)"
    | T (l, k, v, r) -> Printf.sprintf "(T %s %d %d %s)" (aux l) k v (aux r)
  in
  aux t

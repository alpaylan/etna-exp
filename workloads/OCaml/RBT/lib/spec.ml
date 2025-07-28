open Impl
open Util.Runner

type kvlist = (int * int) list
type key = int
type value = int

(* Monad helpers *)
let fromSome d o = match o with None -> d | Some v -> v
let insert' k v t = Some (insert k v t)
let ( =<< ) k m = m >>= k

let rec isBST (t : rbt) : bool =
  let rec every (p : key -> bool) (t : rbt) : bool =
    match t with
    | E -> true
    | T (_, a, x, _, b) -> p x && every p a && every p b
  in
  match t with
  | E -> true
  | T (_, a, x, _, b) ->
      every (( > ) x) a && every (( < ) x) b && isBST a && isBST b

let rec noRedRed (t : rbt) : bool =
  let blackRoot (t : rbt) : bool =
    match t with T (R, _, _, _, _) -> false | _ -> true
  in
  match t with
  | E -> true
  | T (B, a, _, _, b) -> noRedRed a && noRedRed b
  | T (R, a, _, _, b) -> blackRoot a && blackRoot b && noRedRed a && noRedRed b

let consistentBlackHeight (t : rbt) : bool =
  let rec go (t : rbt) : bool * int =
    match t with
    | E -> (true, 1)
    | T (rb, a, _, _, b) ->
        let aBool, aHeight = go a in
        let bBool, bHeight = go b in
        let isBlack (rb : color) : int = match rb with B -> 1 | R -> 0 in
        (aBool && bBool && aHeight = bHeight, aHeight + isBlack rb)
  in
  fst (go t)

let is_rbt (t : rbt) : bool = isBST t && consistentBlackHeight t && noRedRed t

let rec to_list (t : rbt) : kvlist =
  match t with E -> [] | T (_, l, k, v, r) -> to_list l @ [ (k, v) ] @ to_list r

(* -- Validity properties. *)

let prop_InsertValid : rbt * key * value -> test =
 fun (t, k, v) -> is_rbt t ->> is_rbt (insert k v t)

let prop_DeleteValid : rbt * key -> test =
 fun (t, k) -> is_rbt t ->> fromSome false (is_rbt <$> delete k t)

(* ---------- *)

let prop_InsertPost : rbt * key * key * value -> test =
 fun (t, k, k', v) ->
  is_rbt t
  ->> (find k' (insert k v t) = (if k = k' then Some v else find k' t))

let prop_DeletePost : rbt * key * key -> test =
 fun (t, k, k') ->
  is_rbt t
  ->> (find k' <$> delete k t = return (if k = k' then None else find k' t))

(* ---------- *)

(* -- Model-based properties. *)

let delete_key (k : key) (l : kvlist) : kvlist =
  let rec filter f l =
    match l with
    | [] -> []
    | x :: xs -> if f x then x :: filter f xs else filter f xs
  in
  filter (fun x -> not (fst x = k)) l

let rec l_insert (kv : key * value) (l : kvlist) : kvlist =
  match l with
  | [] -> [ kv ]
  | (k, v) :: xs ->
      if fst kv = k then kv :: xs
      else if fst kv < k then kv :: l
      else (k, v) :: l_insert kv xs

let prop_InsertModel : rbt * key * value -> test =
 fun (t, k, v) ->
  is_rbt t
  ->> (to_list (insert k v t)
      = (l_insert (k, v) (delete_key k (to_list t))))

let prop_DeleteModel : rbt * key -> test =
 fun (t, k) ->
  is_rbt t ->> (to_list <$> delete k t = return (delete_key k (to_list t)))

(* ---------- *)

(* -- Metamorphic properties. *)

let ( =~= ) t t' =
  match (t, t') with Some t, Some t' -> to_list t = to_list t' | _ -> false

let prop_InsertInsert : rbt * key * key * value * value -> test =
 fun (t, k, k', v, v') ->
  is_rbt t
  ->> 
   (to_list (insert k v (insert k' v' t))
      = to_list (if k = k' then insert k v t else insert k' v' (insert k v t)))

let prop_InsertDelete : rbt * key * key * value -> test =
 fun (t, k, k', v) ->
  is_rbt t
  ->> 
    match delete k' t with
    | None -> false
    | Some t' -> match delete k' (insert k v t) with
                | None -> false
                | Some t'' ->
                    to_list (insert k v t') = to_list (if k = k' then insert k v t else t'')
  (* (insert k v =<< delete k' t
      =~= if k = k' then insert k v t else delete k' =<< insert k v t) *)

let prop_DeleteInsert : rbt * key * key * value -> test =
 fun (t, k, k', v') ->
  is_rbt t
  ->> 
    match delete k (insert k' v' t) with
    | None -> false
    | Some t' -> match delete k t with  
                | None -> false
                | Some t'' ->
                    let t''' = insert k' v' t'' in
                    to_list t' = to_list (if k = k' then t'' else t''')
  (* (delete k =<< insert k' v' t
      =~= if k = k' then delete k t else insert k' v' =<< delete k t) *)

let prop_DeleteDelete : rbt * key * key -> test =
 fun (t, k, k') ->
  is_rbt t ->> (delete k =<< delete k' t =~= (delete k' =<< delete k t))

(* ---------- *)

let sizeRBT (t : rbt) : int =
  let rec length l = match l with [] -> 0 | _ :: xs -> 1 + length xs in
  length (to_list t)

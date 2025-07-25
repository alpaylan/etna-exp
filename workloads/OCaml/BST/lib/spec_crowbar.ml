open Crowbar
open Impl

let int = int16
let start_time : float option ref = ref None
let end_time : float option ref = ref None
let discards : int ref = ref 0
let generated : int ref = ref 0

(** tapped versions of crowbar's stdlib*)
let add_test ~name gen f =
  add_test ~name gen (fun x ->
      if !start_time = None then start_time := Some (Unix.gettimeofday ());
      incr generated;
      f x)

let check b =
  if not b then end_time := Some (Unix.gettimeofday ());
  check b

let guard b =
  if not b then incr discards;
  guard b

(** Actual properties *)

let prop_C_InsertValid gen =
  add_test ~name:"C_InsertValid" [ gen; int; int ] (fun t k v ->
      guard (is_bst t);
      check (is_bst (insert k v t)))

let prop_C_DeleteValid gen =
  add_test ~name:"C_DeleteValid" [ gen; int ] (fun t k ->
      guard (is_bst t);
      check (is_bst (delete k t)))

let prop_C_UnionValid gen =
  add_test ~name:"C_UnionValid" [ gen; gen ] (fun t t' ->
      guard (is_bst t);
      guard (is_bst t');
      check (is_bst (union t t')))

let prop_C_InsertPost gen =
  add_test ~name:"C_InsertPost" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_bst t);
      check (find k' (insert k v t) = if k = k' then Some v else find k' t))

let prop_C_DeletePost gen =
  add_test ~name:"C_DeletePost" [ gen; int; int ] (fun t k k' ->
      guard (is_bst t);
      check (find k' (delete k t) = if k = k' then None else find k' t))

let prop_C_UnionPost gen =
  add_test ~name:"C_UnionPost" [ gen; gen; int ] (fun t t' k ->
      guard (is_bst t);
      guard (is_bst t');
      check
        (find k (union t t')
        =
        match (find k t, find k t') with
        | None, None -> None
        | None, Some v -> Some v
        | Some v, _ -> Some v))

let prop_C_InsertModel gen =
  add_test ~name:"C_InsertModel" [ gen; int; int ] (fun t k v ->
      guard (is_bst t);
      check (to_list (insert k v t) = l_insert k v (delete_key k (to_list t))))

let prop_C_DeleteModel gen =
  add_test ~name:"C_DeleteModel" [ gen; int ] (fun t k ->
      guard (is_bst t);
      check (to_list (delete k t) = delete_key k (to_list t)))

let prop_C_UnionModel gen =
  add_test ~name:"C_UnionModel" [ gen; gen ] (fun t t' ->
      guard (is_bst t);
      guard (is_bst t');
      check
        (to_list (union t t') = List.sort_uniq compare (to_list t @ to_list t')))

let prop_C_InsertInsert gen =
  add_test ~name:"C_InsertInsert" [ gen; int; int; int; int ]
    (fun t k k' v v' ->
      guard (is_bst t);
      check
        (insert k v (insert k' v' t)
        === if k = k' then insert k v t else insert k' v' (insert k v t)))

let prop_C_InsertDelete gen =
  add_test ~name:"C_InsertDelete" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_bst t);
      check
        (insert k v (delete k' t)
        === if k = k' then insert k v t else delete k' (insert k v t)))

let prop_C_InsertUnion gen =
  add_test ~name:"C_InsertUnion" [ gen; gen; int; int ] (fun t t' k v ->
      guard (is_bst t);
      guard (is_bst t');
      check (insert k v (union t t') === union (insert k v t) t'))

let prop_C_DeleteUnion gen =
  add_test ~name:"C_DeleteUnion" [ gen; gen; int ] (fun t t' k ->
      guard (is_bst t);
      guard (is_bst t');
      check (delete k (union t t') === union (delete k t) t'))

let prop_C_DeleteInsert gen =
  add_test ~name:"Q_DeleteInsert" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_bst t);
      check
        (delete k (insert k' v t)
        === if k = k' then delete k t else insert k' v (delete k t)))

let prop_C_DeleteDelete gen =
  add_test ~name:"Q_DeleteDelete" [ gen; int; int ] (fun t k k' ->
      guard (is_bst t);
      check (delete k (delete k' t) === delete k' (delete k t)))

let prop_C_UnionDeleteInsert gen =
  add_test ~name:"Q_UnionDeleteInsert" [ gen; gen; int; int ] (fun t t' k v ->
      guard (is_bst t);
      guard (is_bst t');
      check (union (delete k t) (insert k v t') === insert k v (union t t')))

let prop_C_UnionUnionIdem gen =
  add_test ~name:"C_UnionUnionIdem" [ gen ] (fun t ->
      guard (is_bst t);
      check (union t t === t))

let prop_C_UnionUnionAssoc gen =
  add_test ~name:"C_UnionUnionAssoc" [ gen; gen; gen ] (fun t1 t2 t3 ->
      guard (is_bst t1);
      guard (is_bst t2);
      guard (is_bst t3);
      check (union (union t1 t2) t3 = union t1 (union t2 t3)))

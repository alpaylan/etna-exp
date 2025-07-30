open Crowbar
open Impl
open Spec

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
      guard (is_rbt t);
      check (is_rbt (insert k v t)))

let prop_C_DeleteValid gen =
  add_test ~name:"C_DeleteValid" [ gen; int ] (fun t k ->
      guard (is_rbt t);
      check (match delete k t with
      | None -> false
      | Some t' -> is_rbt t'))

let prop_C_InsertPost gen =
  add_test ~name:"C_InsertPost" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_rbt t);
      check (find k' (insert k v t) = if k = k' then Some v else find k' t))

let prop_C_DeletePost gen =
  add_test ~name:"C_DeletePost" [ gen; int; int ] (fun t k k' ->
      guard (is_rbt t);
      check (match delete k t with
              | None -> false
              | Some t' -> find k' t' = if k = k' then None else find k' t))

let prop_C_InsertModel gen =
  add_test ~name:"C_InsertModel" [ gen; int; int ] (fun t k v ->
      guard (is_rbt t);
      check (to_list (insert k v t) = l_insert (k, v) (delete_key k (to_list t))))

let prop_C_DeleteModel gen =
  add_test ~name:"C_DeleteModel" [ gen; int ] (fun t k ->
      guard (is_rbt t);
      check (match delete k t with
            | None -> false
            | Some t' -> to_list t' = delete_key k (to_list t)))

let prop_C_InsertInsert gen =
  add_test ~name:"C_InsertInsert" [ gen; int; int; int; int ]
    (fun t k k' v v' ->
      guard (is_rbt t);
      check
        (insert k v (insert k' v' t)
        = if k = k' then insert k v t else insert k' v' (insert k v t)))

let prop_C_InsertDelete gen =
  add_test ~name:"C_InsertDelete" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_rbt t);
      check
        ( match delete k' t with
          | None -> false
          | Some t' -> match delete k' (insert k v t) with
                      | None -> false
                      | Some t'' ->
                          to_list (insert k v t') = to_list (if k = k' then insert k v t else t'')))

let prop_C_DeleteInsert gen =
  add_test ~name:"Q_DeleteInsert" [ gen; int; int; int ] (fun t k k' v ->
      guard (is_rbt t);
      check
        (match delete k (insert k' v t) with
        | None -> false
        | Some t' -> match delete k t with  
                    | None -> false
                    | Some t'' ->
                        let t''' = insert k' v t'' in
                        to_list t' = to_list (if k = k' then t'' else t''')))

let prop_C_DeleteDelete gen =
  add_test ~name:"Q_DeleteDelete" [ gen; int; int ] (fun t k k' ->
      guard (is_rbt t);
      check (delete k =<< delete k' t =~= (delete k' =<< delete k t)))
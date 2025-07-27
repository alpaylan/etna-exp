open Impl
open QCheck2
open QCheck2.Test
open QCheck2.Gen

type test_result = {
  generated : int; (* Number of tests generated *)
  discards : int; (* Number of precondition discards *)
  passed : bool; (* Whether the test passed *)
  elapsed_s : float; (* Time taken for the test in microseconds *)
}

(** All these properties are generalized on the specific QCheck generator *)
let make ~name gen f ~seed =
  let start_time = ref 0. in
  let end_time = ref 0. in
  fun () ->
    ( make_cell ~name ~count:10000000 gen f |> fun c ->
      start_time := Unix.gettimeofday ();
      check_cell c ~rand:(Random.State.make [| seed |]) )
    |> fun result ->
    end_time := Unix.gettimeofday ();
    let elapsed_s = !end_time -. !start_time in
    let open QCheck2.TestResult in
    (* todo: add counterexample printing here *)
    {
      generated = get_count_gen result;
      discards = get_count_gen result - get_count result;
      passed = is_success result;
      elapsed_s;
    }

let int = small_int

(** -- Invariants *)

(** -- Validity properties *)
let prop_Q_InsertValid gen seed =
  make ~name:"Q_InsertValid" (tup3 gen int int)
    (fun (t, k, v) ->
      assume (is_bst t);
      is_bst (insert k v t))
    ~seed

let prop_Q_DeleteValid gen seed =
  make ~name:"Q_DeleteValid" (tup2 gen int)
    (fun (t, k) ->
      assume (is_bst t);
      is_bst (delete k t))
    ~seed

let prop_Q_UnionValid gen seed =
  make ~name:"Q_UnionValid" (tup2 gen gen)
    (fun (t, t') ->
      assume (is_bst t);
      assume (is_bst t');
      is_bst (union t t'))
    ~seed

(** -- Postcondition properties *)
let prop_Q_InsertPost gen seed =
  make ~name:"Q_InsertPost" (tup4 gen int int int)
    (fun (t, k, k', v) ->
      assume (is_bst t);
      find k' (insert k v t) = if k = k' then Some v else find k' t)
    ~seed

let prop_Q_DeletePost gen seed =
  make ~name:"Q_DeletePost" (tup3 gen int int)
    (fun (t, k, k') ->
      assume (is_bst t);
      find k' (delete k t) = if k = k' then None else find k' t)
    ~seed

let prop_Q_UnionPost gen seed =
  make ~name:"Q_UnionPost" (tup3 gen gen int)
    (fun (t, t', k) ->
      assume (is_bst t);
      assume (is_bst t');
      find k (union t t')
      =
      (* First tree takes precedence *)
      match (find k t, find k t') with
      | None, None -> None
      | None, Some v -> Some v
      | Some v, _ -> Some v)
    ~seed

(** -- Model-based properties *)
let prop_Q_InsertModel gen seed =
  make ~name:"Q_InsertModel" (tup3 gen int int)
    (fun (t, k, v) ->
      assume (is_bst t);
      to_list (insert k v t) = l_insert k v (delete_key k (to_list t)))
    ~seed

let prop_Q_DeleteModel gen seed =
  make ~name:"Q_DeleteModel" (tup2 gen int)
    (fun (t, k) ->
      assume (is_bst t);
      to_list (delete k t) = delete_key k (to_list t))
    ~seed

let prop_Q_UnionModel gen seed =
  make ~name:"Q_UnionModel" (tup2 gen gen)
    (fun (t, t') ->
      assume (is_bst t);
      assume (is_bst t');
      to_list (union t t')
      = List.sort compare (l_union (to_list t) (to_list t')))
    ~seed

(** Metamorphic properties *)
let prop_Q_InsertInsert gen seed =
  make ~name:"Q_InsertInsert" (tup5 gen int int int int)
    (fun (t, k, k', v, v') ->
      assume (is_bst t);
      insert k v (insert k' v' t)
      === if k = k' then insert k v t else insert k' v' (insert k v t))
    ~seed

let prop_Q_InsertDelete gen seed =
  make ~name:"Q_InsertDelete" (tup4 gen int int int)
    (fun (t, k, k', v) ->
      assume (is_bst t);
      insert k v (delete k' t)
      === if k = k' then insert k v t else delete k' (insert k v t))
    ~seed

let prop_Q_InsertUnion gen seed =
  make ~name:"Q_InsertUnion" (tup4 gen gen int int) (fun (t, t', k, v) ->
      assume (is_bst t);
      assume (is_bst t');
      insert k v (union t t') === union (insert k v t) t')
  ~seed

let prop_Q_DeleteUnion gen seed =
  make ~name:"Q_DeleteUnion" (tup3 gen gen int) (fun (t, t', k) ->
      assume (is_bst t);
      assume (is_bst t');
      delete k (union t t') === union (delete k t) (delete k t'))
  ~seed

let prop_Q_DeleteInsert gen seed =
  make ~name:"Q_DeleteInsert" (tup4 gen int int int) (fun (t, k, k', v) ->
      assume (is_bst t);
      delete k (insert k' v t)
      === if k = k' then delete k t else insert k' v (delete k t))
  ~seed

let prop_Q_DeleteDelete gen seed =
  make ~name:"Q_DeleteDelete" (tup3 gen int int) (fun (t, k, k') ->
      assume (is_bst t);
      delete k (delete k' t) === delete k' (delete k t))
  ~seed

let prop_Q_UnionDeleteInsert gen seed =
  make ~name:"Q_UnionDeleteInsert" (tup4 gen gen int int) (fun (t, t', k, v) ->
      assume (is_bst t);
      assume (is_bst t');
      union (delete k t) (insert k v t') === insert k v (union t t'))
  ~seed

let prop_Q_UnionUnionIdem gen seed =
  make ~name:"Q_UnionUnionIdem" gen (fun t ->
      assume (is_bst t);
      union t t === t)
  ~seed

let prop_Q_UnionUnionAssoc gen seed =
  make ~name:"Q_UnionUnionAssoc" (tup3 gen gen gen) (fun (t1, t2, t3) ->
      assume (is_bst t1);
      assume (is_bst t2);
      assume (is_bst t3);
      union (union t1 t2) t3 = union t1 (union t2 t3))
  ~seed

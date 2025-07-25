open QCheck2
open Impl

(* Helper function to insert a value into a BST *)
let rec insert k v = function
  | E -> T (E, k, v, E)
  | T (l, k', v', r) as t ->
      if k < k' then T (insert k v l, k', v', r)
      else if k > k' then T (l, k', v', insert k v r)
      else t (* No duplicates *)

(* Generate a BST from a list of unique integers *)

let gen_Q_Bespoke =
  let open Gen in
  let* xs =
    list_size (int_bound 20) (pair (int_bound 100) (int_bound 100))
  in
  let xs = List.sort_uniq (fun (k1, _) (k2, _) -> compare k1 k2) xs in
  let tree = List.fold_left (fun acc (k, v) -> insert k v acc) E xs in
  return tree


(* let gen_Q_Bespoke =
  let open Gen in
  let* t =
    sized (fun n ->
        if n = 0 then return E
        else
          let* xs =
            list_size (int_bound n) (pair (int_bound 100) (int_bound 100))
          in
          let xs = List.sort_uniq (fun (k1, _) (k2, _) -> compare k1 k2) xs in
          let rec build_bst = function
            | [] -> E
            | (k, v) :: rest ->
                let left, right = List.partition (fun (k', _) -> k' < k) rest in
                T (build_bst left, k, v, build_bst right)
          in
          return (build_bst xs))
  in
  return t *)

open QCheck2
open QCheck2.Test
open QCheck2.Gen

type test_result = {
  generated : int; (* Number of tests generated *)
  discards : int; (* Number of precondition discards *)
  passed : bool; (* Whether the test passed *)
  elapsed_s : float; (* Time taken for the test in microseconds *)
}

open Printf

type json_entry = {
  time : float;
  value : string;
}

let quick_sample ~count ~gen ~print ~prop ~seed =
  let gen_count = ref 0 in
  let start_time = Unix.gettimeofday () in
  let collected = ref [] in

  let wrap_checker checker =
    fun x ->
      let now = Unix.gettimeofday () in
      let rel_time = now -. start_time in
      incr gen_count;
      collected := { time = rel_time; value = print x } :: !collected;
      checker x
  in

  let cell = Test.make_cell ~count gen (wrap_checker prop) in
  let _res = Test.check_cell cell ~rand:(Random.State.make [| seed |]) in

  let entries =
    !collected
    |> List.rev
    |> List.map (fun { time; value } ->
         sprintf {|{ "time": "%.9fs", "value": "%s" }|} time (String.escaped value))
  in
  printf "[%s]\n" (String.concat ",\n" entries)
;;

(** All these properties are generalized on the specific QCheck generator *)
let make ~name gen f =
  let start_time = ref 0. in
  let end_time = ref 0. in
  fun () ->
    ( make_cell ~name ~count:10000000 gen f |> fun c ->
      start_time := Unix.gettimeofday ();
      check_cell c )
    |> fun result ->
    end_time := Unix.gettimeofday ();
    let elapsed_s = !end_time -. !start_time in
    let open QCheck2.TestResult in
    {
      generated = get_count_gen result;
      discards = get_count_gen result - get_count result;
      passed = is_success result;
      elapsed_s;
    }

let int = small_int

(** -- Invariants *)

module TuplePrinter = struct

  let tree = BST.Impl.to_string
  let pint = string_of_int

  let p2 (p1, p2) (x1, x2) =
    Printf.sprintf "(%s %s)" (p1 x1) (p2 x2)

  let p3 (p1, p2, p3) (x1, x2, x3) =
    Printf.sprintf "(%s %s %s)" (p1 x1) (p2 x2) (p3 x3)

  let p4 (p1, p2, p3, p4) (x1, x2, x3, x4) =
    Printf.sprintf "(%s %s %s %s)" (p1 x1) (p2 x2) (p3 x3) (p4 x4)

  let p5 (p1, p2, p3, p4, p5) (x1, x2, x3, x4, x5) =
    Printf.sprintf "(%s %s %s %s %s)" (p1 x1) (p2 x2) (p3 x3) (p4 x4) (p5 x5)
end

open TuplePrinter

(** -- Validity properties *)
let prop_Q_InsertValid ~gen ~count ~seed =
  quick_sample
      ~count
      ~gen:(tup3 gen int int) 
      ~print:(p3 (tree, pint, pint))
      ~prop:(fun (t, k, v) ->
        let _ = (t, k, v) in
        true)
      ~seed

let prop_Q_DeleteValid ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup2 gen int)
    ~print:(p2 (tree, pint))
    ~prop:(fun (t, k) ->
      let _ = (t, k) in
      true)
    ~seed

let prop_Q_UnionValid ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup2 gen gen)
    ~print:(p2 (tree, tree))
    ~prop:(fun (t, t') ->
      let _ = (t, t') in
      true)
    ~seed

(** -- Postcondition properties *)
let prop_Q_InsertPost ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup4 gen int int int)
    ~print:(p4 (tree, pint, pint, pint))
    ~prop:(fun (t, k, k', v) ->
      let _ = (t, k, k', v) in
      true)
    ~seed

let prop_Q_DeletePost ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen int int)
    ~print:(p3 (tree, pint, pint))
    ~prop:(fun (t, k, k') ->
      let _ = (t, k, k') in
      true)
    ~seed

let prop_Q_UnionPost ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen gen int)
    ~print:(p3 (tree, tree, pint))
    ~prop:(fun (t, t', k) ->
      let _ = (t, t', k) in
      true)
    ~seed

(** -- Model-based properties *)
let prop_Q_InsertModel ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen int int)
    ~print:(p3 (tree, pint, pint))
    ~prop:(fun (t, k, v) ->
      let _ = (t, k, v) in
      true)
    ~seed

let prop_Q_DeleteModel ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup2 gen int)
    ~print:(p2 (tree, pint))
    ~prop:(fun (t, k) ->
      let _ = (t, k) in
      true)
    ~seed

let prop_Q_UnionModel ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup2 gen gen)
    ~print:(p2 (tree, tree))
    ~prop:(fun (t, t') ->
      let _ = (t, t') in
      true)
    ~seed

(** Metamorphic properties *)
let prop_Q_InsertInsert ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup5 gen int int int int)
    ~print:(p5 (tree, pint, pint, pint, pint))
    ~prop:(fun (t, k, k', v, v') ->
      let _ = (t, k, k', v, v') in
      true)
    ~seed

let prop_Q_InsertDelete ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup4 gen int int int)
    ~print:(p4 (tree, pint, pint, pint))
    ~prop:(fun (t, k, k', v) ->
      let _ = (t, k, k', v) in
      true)
    ~seed
let prop_Q_InsertUnion ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup4 gen gen int int)
    ~print:(p4 (tree, tree, pint, pint))
    ~prop:(fun (t, t', k, v) ->
      let _ = (t, t', k, v) in
      true)
    ~seed

let prop_Q_DeleteUnion ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen gen int)
    ~print:(p3 (tree, tree, pint))
    ~prop:(fun (t, t', k) ->
      let _ = (t, t', k) in
      true)
    ~seed

let prop_Q_DeleteInsert ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup4 gen int int int)
    ~print:(p4 (tree, pint, pint, pint))
    ~prop:(fun (t, k, k', v) ->
      let _ = (t, k, k', v) in
      true)
    ~seed

let prop_Q_DeleteDelete ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen int int)
    ~print:(p3 (tree, pint, pint))
    ~prop:(fun (t, k, k') ->
      let _ = (t, k, k') in
      true)
    ~seed

let prop_Q_UnionDeleteInsert ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup4 gen gen int int)
    ~print:(p4 (tree, tree, pint, pint))
    ~prop:(fun (t, t', k, v) ->
      let _ = (t, t', k, v) in
      true)
    ~seed

let prop_Q_UnionUnionIdem ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:gen
    ~print:tree
    ~prop:(fun t ->
      let _ = t in
      true)
    ~seed

let prop_Q_UnionUnionAssoc ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:(tup3 gen gen gen)
    ~print:(p3 (tree, tree, tree))
    ~prop:(fun (t1, t2, t3) ->
      let _ = (t1, t2, t3) in
      true)
    ~seed

let print_result (r : test_result) =
  let status = if r.passed then "Finished" else "Failed" in
  Printf.printf
    {|[|{
    "discards": %d,
    "tests": %d,
    "status": "%s",
    "time": "%fs"
  }|]|}
    r.discards r.generated status r.elapsed_s;
  print_endline ""

let run
    (make_test :
      BST.Impl.t QCheck2.Gen.t -> unit -> test_result)
    (gen : BST.Impl.t QCheck2.Gen.t) : unit =
  make_test gen () |> print_result

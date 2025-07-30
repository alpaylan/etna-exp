open Impl
open Spec
open QCheck2
open QCheck2.Test

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

let int = Gen.small_int

(** -- Invariants *)

(** -- Validity properties *)
let prop_Q_SinglePreserve gen seed =
  make ~name:"Q_SinglePreserve" gen
    (fun e ->
      assume (typechecks e);
      Option.value
        (mt e >>= fun t -> Some (mtypeCheck (pstep e) t))
        ~default:true)
    ~seed

let prop_Q_MultiPreserve gen seed =
  make ~name:"Q_MultiPreserve" gen
    (fun e ->
      assume (typechecks e);
      Option.value
        (mt e >>= fun t -> Some (mtypeCheck (multistep 40 pstep e) t))
        ~default:true)
    ~seed

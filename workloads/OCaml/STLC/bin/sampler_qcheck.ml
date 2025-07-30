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
    (* reverse the list *)
    |> List.rev
    (* turn the cumulative list to individual times *)
    |> (
      let last_time = ref 0. in
      List.map (fun { time; value } ->
        let elapsed = time -. !last_time in
        last_time := time;
        { time = elapsed; value })
    )
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

(** -- Validity properties *)
let prop_Q_SinglePreserve ~gen ~count ~seed =
  quick_sample
      ~count
      ~gen:gen
      ~print:Stlc.Impl.to_string
      ~prop:(fun e ->
        let _ = e in
        true)
      ~seed

let prop_Q_MultiPreserve ~gen ~count ~seed =
  quick_sample
    ~count
    ~gen:gen
    ~print:Stlc.Impl.to_string
    ~prop:(fun e ->
      let _ = e in
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
      Stlc.Impl.expr QCheck2.Gen.t -> unit -> test_result)
    (gen : Stlc.Impl.expr QCheck2.Gen.t) : unit =
  make_test gen () |> print_result

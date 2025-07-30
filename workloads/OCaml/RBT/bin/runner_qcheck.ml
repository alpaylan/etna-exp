let print_result (r : Rbt.Spec_qcheck.test_result) =
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
    ~prop:
      (make_test :
        Rbt.Impl.rbt QCheck2.Gen.t -> int -> unit -> Rbt.Spec_qcheck.test_result)
    ~(gen : Rbt.Impl.rbt QCheck2.Gen.t)
    ~(seed: int) : unit =
  make_test gen seed () |> print_result

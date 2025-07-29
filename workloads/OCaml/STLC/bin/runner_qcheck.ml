let print_result (r : Stlc.Spec_qcheck.test_result) =
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
        Stlc.Impl.expr QCheck2.Gen.t -> int -> unit -> Stlc.Spec_qcheck.test_result)
    ~(gen : Stlc.Impl.expr QCheck2.Gen.t)
    ~(seed: int) : unit =
  make_test gen seed () |> print_result

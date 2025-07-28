open Rbt.Spec_crowbar

let run prop gen =
  prop gen;

  Crowbar.add_test ~name:"dummy" [ Crowbar.bool ] (fun _ ->
      let status = if !end_time <> None then "Failed" else "Finished" in
      Printf.printf
        {|[|{
        "discards": %d,
        "tests": %d,
        "status": "%s",
        "time": "%fs"
      }|]|}
        !discards !generated status
        (Option.get !end_time -. Option.get !start_time);
      print_endline "";
      Crowbar.check false)

From QuickChick Require Import QuickChick.

Set Warnings "-extraction-opaque-accessed,-extraction".

From RBT Require Import BespokeGenerator.

Axiom num_tests : nat. 
Extract Constant num_tests => "max_int".

Definition qctest_test_prop_InsertValid := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertValid))) ++ "}|]")).
Definition qctest_test_prop_DeleteValid := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteValid))) ++ "}|]")).
Definition qctest_test_prop_InsertPost := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertPost))) ++ "}|]")).
Definition qctest_test_prop_DeletePost := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeletePost))) ++ "}|]")).
Definition qctest_test_prop_InsertModel := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertModel))) ++ "}|]")).
Definition qctest_test_prop_DeleteModel := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteModel))) ++ "}|]")).
Definition qctest_test_prop_InsertInsert := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertInsert))) ++ "}|]")).
Definition qctest_test_prop_InsertDelete := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertDelete))) ++ "}|]")).
Definition qctest_test_prop_DeleteInsert := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteInsert))) ++ "}|]")).
Definition qctest_test_prop_DeleteDelete := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteDelete))) ++ "}|]")).

Parameter OCamlString : Type.
Extract Constant OCamlString => "string".

Axiom qctest_map : OCamlString -> unit.
Extract Constant qctest_map => "
fun test_name ->
  let test_map = [
    (""InsertValid"", qctest_test_prop_InsertValid);
    (""DeleteValid"", qctest_test_prop_DeleteValid);
    (""InsertPost"", qctest_test_prop_InsertPost);
    (""DeletePost"", qctest_test_prop_DeletePost);
    (""InsertModel"", qctest_test_prop_InsertModel);
    (""DeleteModel"", qctest_test_prop_DeleteModel);
    (""InsertInsert"", qctest_test_prop_InsertInsert);
    (""InsertDelete"", qctest_test_prop_InsertDelete);
    (""DeleteInsert"", qctest_test_prop_DeleteInsert);
    (""DeleteDelete"", qctest_test_prop_DeleteDelete)
  ] in
  let test = List.assoc test_name test_map in
  test ()


let () =
Sys.argv.(1) |> qctest_map
".


Extraction "BespokeGenerator_test_runner.ml" qctest_test_prop_InsertValid qctest_test_prop_DeleteValid qctest_test_prop_InsertPost qctest_test_prop_DeletePost qctest_test_prop_InsertModel qctest_test_prop_DeleteModel qctest_test_prop_InsertInsert qctest_test_prop_InsertDelete qctest_test_prop_DeleteInsert qctest_test_prop_DeleteDelete  qctest_map.

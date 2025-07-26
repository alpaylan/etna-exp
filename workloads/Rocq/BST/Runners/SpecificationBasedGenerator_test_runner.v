From QuickChick Require Import QuickChick.

Set Warnings "-extraction-opaque-accessed,-extraction".

From BST Require Import SpecificationBasedGenerator.

Axiom num_tests : nat. 
Extract Constant num_tests => "max_int".

Definition qctest_test_prop_InsertValid := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertValid))) ++ "}|]")).
Definition qctest_test_prop_DeleteValid := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteValid))) ++ "}|]")).
Definition qctest_test_prop_UnionValid := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_UnionValid))) ++ "}|]")).
Definition qctest_test_prop_InsertPost := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertPost))) ++ "}|]")).
Definition qctest_test_prop_DeletePost := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeletePost))) ++ "}|]")).
Definition qctest_test_prop_UnionPost := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_UnionPost))) ++ "}|]")).
Definition qctest_test_prop_InsertModel := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertModel))) ++ "}|]")).
Definition qctest_test_prop_DeleteModel := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteModel))) ++ "}|]")).
Definition qctest_test_prop_UnionModel := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_UnionModel))) ++ "}|]")).
Definition qctest_test_prop_InsertInsert := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertInsert))) ++ "}|]")).
Definition qctest_test_prop_InsertDelete := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertDelete))) ++ "}|]")).
Definition qctest_test_prop_InsertUnion := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_InsertUnion))) ++ "}|]")).
Definition qctest_test_prop_DeleteInsert := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteInsert))) ++ "}|]")).
Definition qctest_test_prop_DeleteDelete := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteDelete))) ++ "}|]")).
Definition qctest_test_prop_DeleteUnion := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_DeleteUnion))) ++ "}|]")).
Definition qctest_test_prop_UnionDeleteInsert := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_UnionDeleteInsert))) ++ "}|]")).
Definition qctest_test_prop_UnionUnionIdem := (fun _ : unit => print_extracted_coq_string ("[|{" ++ show (withTime(fun tt => (quickCheckWith (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) test_prop_UnionUnionIdem))) ++ "}|]")).

Parameter OCamlString : Type.
Extract Constant OCamlString => "string".

Axiom qctest_map : OCamlString -> unit.
Extract Constant qctest_map => "
fun test_name ->
  let test_map = [
    (""InsertValid"", qctest_test_prop_InsertValid);
    (""DeleteValid"", qctest_test_prop_DeleteValid);
    (""UnionValid"", qctest_test_prop_UnionValid);
    (""InsertPost"", qctest_test_prop_InsertPost);
    (""DeletePost"", qctest_test_prop_DeletePost);
    (""UnionPost"", qctest_test_prop_UnionPost);
    (""InsertModel"", qctest_test_prop_InsertModel);
    (""DeleteModel"", qctest_test_prop_DeleteModel);
    (""UnionModel"", qctest_test_prop_UnionModel);
    (""InsertInsert"", qctest_test_prop_InsertInsert);
    (""InsertDelete"", qctest_test_prop_InsertDelete);
    (""InsertUnion"", qctest_test_prop_InsertUnion);
    (""DeleteInsert"", qctest_test_prop_DeleteInsert);
    (""DeleteDelete"", qctest_test_prop_DeleteDelete);
    (""DeleteUnion"", qctest_test_prop_DeleteUnion);
    (""UnionDeleteInsert"", qctest_test_prop_UnionDeleteInsert);
    (""UnionUnionIdem"", qctest_test_prop_UnionUnionIdem)
  ] in
  let test = List.assoc test_name test_map in
  test ()


let () =
Sys.argv.(1) |> qctest_map
".


Extraction "SpecificationBasedGenerator_test_runner.ml" qctest_test_prop_InsertValid qctest_test_prop_DeleteValid qctest_test_prop_UnionValid qctest_test_prop_InsertPost qctest_test_prop_DeletePost qctest_test_prop_UnionPost qctest_test_prop_InsertModel qctest_test_prop_DeleteModel qctest_test_prop_UnionModel qctest_test_prop_InsertInsert qctest_test_prop_InsertDelete qctest_test_prop_InsertUnion qctest_test_prop_DeleteInsert qctest_test_prop_DeleteDelete qctest_test_prop_DeleteUnion qctest_test_prop_UnionDeleteInsert qctest_test_prop_UnionUnionIdem  qctest_map.

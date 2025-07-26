From QuickChick Require Import QuickChick.

Set Warnings "-extraction-opaque-accessed,-extraction".

From BST Require Import BespokeGenerator.
From BST Require Import Impl.

Local Open Scope string_scope.

#[local] Instance showTimedResult {A: Type} `{Show A} : Show (@TimedResult A) := {|
  show result := 
    let '(TResult result time start ending) := result in
     "{ ""time"": """ ++ show time ++ """, ""value"": """ ++ show result ++ """ }"
|}.

Definition showTimedResultList {A: Type} `{Show A} (results : list (@TimedResult A)) : string :=
  "[" ++ String.concat ", " (List.map (fun r => show r) results) ++ "]".


Inductive Args1 {A : Type} : Type :=
  | Args1Mk : A -> Args1.

#[local] Instance ShowArgs1 {A} `{Show A} : Show (@Args1 A) :=
  { show := fun '(Args1Mk a) => "(" ++ show a ++ ")" }.


Inductive Args2 {A B : Type} : Type :=
  | Args2Mk : A -> B -> Args2.

#[local] Instance ShowArgs2 {A B} `{Show A} `{Show B} : Show (@Args2 A B) :=
  { show := fun '(Args2Mk a b) => "(" ++ show a ++ " " ++ show b ++ ")" }.


Inductive Args3 {A B C : Type} : Type :=
  | Args3Mk : A -> B -> C -> Args3.

#[local] Instance ShowArgs3 {A B C} `{Show A} `{Show B} `{Show C} : Show (@Args3 A B C) :=
  { show := fun '(Args3Mk a b c) => "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")" }.


Inductive Args4 {A B C D : Type} : Type :=
  | Args4Mk : A -> B -> C -> D -> Args4.

#[local] Instance ShowArgs4 {A B C D} `{Show A} `{Show B} `{Show C} `{Show D} : Show (@Args4 A B C D) :=
  { show := fun '(Args4Mk a b c d) => "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")" }.


Inductive Args5 {A B C D E : Type} : Type :=
  | Args5Mk : A -> B -> C -> D -> E -> Args5.

#[local] Instance ShowArgs5 {A B C D E} `{Show A} `{Show B} `{Show C} `{Show D} `{Show E} : Show (@Args5 A B C D E) :=
  { show := fun '(Args5Mk a b c d e) => "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ ")" }.

  
Definition sample_InsertValid : G Args3 := 
  bindGen bespoke (fun t =>
  bindGen arbitrary (fun k =>
  bindGen arbitrary (fun v =>
  ret (Args3Mk t k v)))).

Definition qctest_sample_InsertValid := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertValid))).

Definition sample_DeleteValid : G Args2  :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat) =>
  ret(Args2Mk t k))).

Definition qctest_sample_DeleteValid := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeleteValid))).


Definition sample_UnionValid  : G Args2 :=
  bindGen bespoke (fun (t1: Tree)  =>
  bindGen bespoke (fun (t2: Tree) =>
  ret (Args2Mk t1 t2))).
 
Definition qctest_sample_UnionValid := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionValid))).


Definition sample_InsertPost    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat)  =>
  bindGen arbitrary (fun (v: nat) =>
  ret (Args4Mk t k k' v))))).

Definition qctest_sample_InsertPost := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertPost))).


Definition sample_DeletePost    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat) =>
  ret (Args3Mk t k k'))))
.

Definition qctest_sample_DeletePost := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeletePost))).


Definition sample_UnionPost   :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen bespoke (fun (t': Tree)  =>
  bindGen arbitrary (fun (k: nat) =>
  ret (Args3Mk t t' k))))
.

Definition qctest_sample_UnionPost := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionPost))).

Definition sample_InsertModel   :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (v: nat) =>
  ret (Args3Mk t k v))))
.

Definition qctest_sample_InsertModel := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertModel))).


Definition sample_DeleteModel   :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat) =>
  ret (Args2Mk t k)))
.

Definition qctest_sample_DeleteModel := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeleteModel))).


Definition sample_UnionModel    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen bespoke (fun (t': Tree) =>
  ret (Args2Mk t t'))).

Definition qctest_sample_UnionModel := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionModel))).


Definition sample_InsertInsert    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat)  =>
  bindGen arbitrary (fun (v: nat)  =>
  bindGen arbitrary (fun (v': nat) =>
  ret (Args5Mk t k k' v v'))))))
.

Definition qctest_sample_InsertInsert := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertInsert))).


Definition sample_InsertDelete    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat)  =>
  bindGen arbitrary (fun (v: nat) =>
  ret (Args4Mk t k k' v)))))
.

Definition qctest_sample_InsertDelete := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertDelete))).


Definition sample_InsertUnion   :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen bespoke (fun (t': Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (v: nat) =>
  ret (Args4Mk t t' k v)))))
.

Definition qctest_sample_InsertUnion := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_InsertUnion))).


Definition sample_DeleteInsert    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat)  =>
  bindGen arbitrary (fun (v': nat) =>
  ret (Args4Mk t k k' v')))))
.

Definition qctest_sample_DeleteInsert := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeleteInsert))).


Definition sample_DeleteDelete    :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (k': nat) =>
  ret (Args3Mk t k k'))))
.

Definition qctest_sample_DeleteDelete := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeleteDelete))).


Definition sample_DeleteUnion   :=
  bindGen bespoke (fun (t: Tree)  =>
  bindGen bespoke (fun (t': Tree)  =>
  bindGen arbitrary (fun (k: nat) =>
  ret (Args3Mk t t' k))))
.

Definition qctest_sample_DeleteUnion := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_DeleteUnion))).


Definition sample_UnionDeleteInsert   :=
  bindGen bespoke (fun (t :Tree)  =>
  bindGen bespoke (fun (t': Tree)  =>
  bindGen arbitrary (fun (k: nat)  =>
  bindGen arbitrary (fun (v: nat) =>
  ret (Args4Mk t t' k v)))))
.

Definition qctest_sample_UnionDeleteInsert := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionDeleteInsert))).


Definition sample_UnionUnionIdem    :=
  bindGen bespoke (fun (t: Tree) =>
  ret (Args1Mk t))
.

Definition qctest_sample_UnionUnionIdem := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionUnionIdem))).


Definition sample_UnionUnionAssoc   :=
  bindGen bespoke (fun (t1: Tree)  =>
  bindGen bespoke (fun (t2: Tree)  =>
  bindGen bespoke (fun (t3: Tree) =>
  ret (Args3Mk t1 t2 t3))))
.

Definition qctest_sample_UnionUnionAssoc := (fun num_tests: nat => print_extracted_coq_string (showTimedResultList (quickSample (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests) num_tests) sample_UnionUnionAssoc) )). 



Parameter OCamlString : Type.
Extract Constant OCamlString => "string".

Axiom test_map : list (OCamlString * (nat -> unit)).
Extract Constant test_map => "[
    (""InsertValid"", qctest_sample_InsertValid);
    (""DeleteValid"", qctest_sample_DeleteValid);
    (""UnionValid"", qctest_sample_UnionValid);
    (""InsertPost"", qctest_sample_InsertPost);
    (""DeletePost"", qctest_sample_DeletePost);
    (""UnionPost"", qctest_sample_UnionPost);
    (""InsertModel"", qctest_sample_InsertModel);
    (""DeleteModel"", qctest_sample_DeleteModel);
    (""UnionModel"", qctest_sample_UnionModel);
    (""InsertInsert"", qctest_sample_InsertInsert);
    (""InsertDelete"", qctest_sample_InsertDelete);
    (""InsertUnion"", qctest_sample_InsertUnion);
    (""DeleteInsert"", qctest_sample_DeleteInsert);
    (""DeleteDelete"", qctest_sample_DeleteDelete);
    (""DeleteUnion"", qctest_sample_DeleteUnion);
    (""UnionDeleteInsert"", qctest_sample_UnionDeleteInsert);
    (""UnionUnionIdem"", qctest_sample_UnionUnionIdem);
    (""UnionUnionAssoc"", qctest_sample_UnionUnionAssoc)
]".

Axiom qctest_map : OCamlString -> nat -> unit.
Extract Constant qctest_map => "
fun property num_tests ->
  let test = List.assoc property test_map in
  test num_tests


let () =
  let args = Sys.argv in
  if Array.length args <> 3 then
    Printf.eprintf ""Usage: %s <property> <#tests>\n"" args.(0)
  else
    let property = args.(1) in
    let num_tests = int_of_string args.(2) in
    if not (List.mem_assoc property test_map) then
      Printf.eprintf ""Unknown test name: %s\n"" property
    else
      qctest_map property num_tests
".


Extraction "BespokeGenerator_sampler.ml" test_map qctest_map qctest_sample_InsertValid qctest_sample_DeleteValid qctest_sample_UnionValid qctest_sample_InsertPost qctest_sample_DeletePost qctest_sample_UnionPost qctest_sample_InsertModel qctest_sample_DeleteModel qctest_sample_UnionModel qctest_sample_InsertInsert qctest_sample_InsertDelete qctest_sample_InsertUnion qctest_sample_DeleteInsert qctest_sample_DeleteDelete qctest_sample_DeleteUnion qctest_sample_UnionDeleteInsert qctest_sample_UnionUnionIdem qctest_sample_UnionUnionAssoc.

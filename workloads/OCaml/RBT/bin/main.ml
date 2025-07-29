open Cmdliner
(* open QCheck *)
(* open Crowbar *)
(* open Util.Runner
open Util.Io *)
(* open Rbt.Impl
open Rbt.Test *)
open Rbt.Spec_qcheck
(* open Rbt.Spec_crowbar *)
open Rbt.Gen_typebased_qcheck
(* open Rbt.Gen_typebased_crowbar *)
open Rbt.Gen_bespoke_qcheck
(* open Rbt.Gen_bespoke_crowbar *)

(* open Rbt.QcheckType
open Rbt.QcheckBespoke
open Rbt.CrowbarType
open Rbt.CrowbarBespoke
open Rbt.BaseType
open Rbt.BaseBespoke *)

(* RUNNER COMMAND:
   dune exec rnt -- qcheck prop_DeleteValid bespoke out.txt
   dune exec rnt -- qcheck prop_DeleteValid type out.txt
   dune exec rnt -- crowbar prop_DeleteValid bespoke out.txt
   dune exec rnt -- crowbar prop_DeleteValid type out.txt
   dune exec rnt -- afl prop_DeleteValid bespoke out.txt
   dune exec rnt -- afl prop_DeleteValid type out.txt
   dune exec rnt -- base prop_DeleteValid type out
*)

let qcheck_property name =
  match name with
  | "InsertValid" -> prop_Q_InsertValid
  | "DeleteValid" -> prop_Q_DeleteValid
  | "InsertPost" -> prop_Q_InsertPost
  | "DeletePost" -> prop_Q_DeletePost
  | "InsertModel" -> prop_Q_InsertModel
  | "DeleteModel" -> prop_Q_DeleteModel
  | "InsertInsert" -> prop_Q_InsertInsert
  | "InsertDelete" -> prop_Q_InsertDelete
  | "DeleteInsert" -> prop_Q_DeleteInsert
  | "DeleteDelete" -> prop_Q_DeleteDelete
  | _ -> raise (Invalid_argument ("Unknown property: " ^ name))

let qcheck_generator name =
  match name with
  | "typebased" -> gen_Q_TypeBased
  | "bespoke" -> gen_Q_Bespoke
  | _ -> raise (Invalid_argument ("Unknown generator: " ^ name))


(* let properties : (string * rbt property) list =
  [
    ("prop_InsertValid", test_prop_InsertValid);
    ("prop_DeleteValid", test_prop_DeleteValid);
    ("prop_InsertPost", test_prop_InsertPost);
    ("prop_DeletePost", test_prop_DeletePost);
    ("prop_InsertModel", test_prop_InsertModel);
    ("prop_DeleteModel", test_prop_DeleteModel);
    ("prop_InsertInsert", test_prop_InsertInsert);
    ("prop_InsertDelete", test_prop_InsertDelete);
    ("prop_DeleteInsert", test_prop_DeleteInsert);
    ("prop_DeleteDelete", test_prop_DeleteDelete);
  ]

let qstrategies : (string * rbt arbitrary) list =
  [ ("type", qcheck_type); ("bespoke", qcheck_bespoke) ]

let cstrategies : (string * rbt gen) list =
  [ ("type", crowbar_type); ("bespoke", crowbar_bespoke) ]

let bstrategies : (string * rbt basegen) list =
  [ ("type", (module BaseType)); ("bespoke", (module BaseBespoke)) ]

 *)

let main property strategy seed =
  (* Your logic here: select property and generator by name *)
  let (framework, generator) = 
    match String.split_on_char ':' strategy with
    | [ framework; generator ] -> (framework, generator)
    | _ -> failwith "Strategy must be in the form FRAMEWORK:GENERATOR" in
  match framework |> String.lowercase_ascii with
  | "qcheck" -> 
      Runner_qcheck.run
        ~prop: (qcheck_property property)
        ~gen: (qcheck_generator generator)
        ~seed
  (* | "crowbar" ->
      (* have to do this because crowbar reads command lines args *)
      Random.self_init ();
      Sys.argv.(1) <- "--repeat=10000000";
      Sys.argv.(2) <- Printf.sprintf "--seed=%d" (Random.int 1000000);
      Runner_crowbar.run
        (crowbar_property property)
        (crowbar_generator generator) *)
  | _ -> failwith "framework must be either 'qcheck' or 'crowbar'"

(** *)

(** Cmdliner stuff *)

(** | *)

(** v *)

let _ = Random.self_init ()

let property_arg =
  let doc = "Name of the property test to run." in
  Arg.(required & opt (some string) None & info [ "property" ] ~docv:"PROPERTY" ~doc)

let generator_arg =
  let doc = "Name of the strategy to use." in
  Arg.(
    required
    & opt (some string) None
    & info [ "strategy" ] ~docv:"FRAMEWORK:GENERATOR" ~doc)

let seed_arg =
  let doc = "Random seed for the generator." in
  Arg.(
    value
    & opt int (Random.int 1000000)
    & info [ "seed" ] ~docv:"SEED" ~doc)

let term = Term.(const main $ property_arg $ generator_arg $ seed_arg)
let () = Cmd.(exit @@ eval (v (info "BST") term))

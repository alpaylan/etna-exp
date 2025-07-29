open Cmdliner
(* open QCheck *)
(* open Crowbar *)
(* open Util.Runner
open Util.Io *)
(* open Stlc.Impl
open Stlc.Test *)
open Stlc.Spec_qcheck
(* open Stlc.Spec_crowbar *)
open Stlc.Gen_typebased_qcheck
(* open Stlc.Gen_typebased_crowbar *)
open Stlc.Gen_bespoke_qcheck
(* open Stlc.Gen_bespoke_crowbar *)

(* open Stlc.QcheckType
open Stlc.QcheckBespoke
open Stlc.CrowbarType
open Stlc.CrowbarBespoke
open Stlc.BaseType
open Stlc.BaseBespoke *)

(* RUNNER COMMAND:
   dune exec stlc -- --strategy=qcheck:bespoke  --property=SinglePreserve 
   dune exec stlc -- --strategy=qcheck:type     --property=SinglePreserve 
   dune exec stlc -- --strategy=crowbar:bespoke --property=SinglePreserve 
   dune exec stlc -- --strategy=crowbar:type    --property=SinglePreserve 
   dune exec stlc -- --strategy=afl:bespoke     --property=SinglePreserve 
   dune exec stlc -- --strategy=afl:type        --property=SinglePreserve 
   dune exec stlc -- --strategy=base:bespoke    --property=SinglePreserve 
   dune exec stlc -- --strategy=base:type       --property=SinglePreserve 
*)

let qcheck_property name =
  match name with
  | "SinglePreserve" -> prop_Q_SinglePreserve
  | "MultiPreserve" -> prop_Q_MultiPreserve
  | _ -> raise (Invalid_argument ("Unknown property: " ^ name))

let qcheck_generator name =
  match name with
  | "typebased" -> gen_Q_TypeBased
  | "bespoke" -> gen_Q_Bespoke
  | _ -> raise (Invalid_argument ("Unknown generator: " ^ name))

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

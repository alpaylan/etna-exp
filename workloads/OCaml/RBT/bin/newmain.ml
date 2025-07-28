open Cmdliner
open Rbt.Spec_qcheck
open Rbt.Spec_crowbar
open Rbt.Gen_typebased_qcheck
open Rbt.Gen_typebased_crowbar
open Rbt.Gen_bespoke_qcheck
open Rbt.Gen_bespoke_crowbar

let qcheck_property name =
  match name with
  | "InsertValid" -> prop_Q_InsertValid
  | "DeleteValid" -> prop_Q_DeleteValid
  | "UnionValid" -> prop_Q_UnionValid
  | "InsertPost" -> prop_Q_InsertPost
  | "DeletePost" -> prop_Q_DeletePost
  | "UnionPost" -> prop_Q_UnionPost
  | "InsertModel" -> prop_Q_InsertModel
  | "DeleteModel" -> prop_Q_DeleteModel
  | "UnionModel" -> prop_Q_UnionModel
  | "InsertInsert" -> prop_Q_InsertInsert
  | "InsertDelete" -> prop_Q_InsertDelete
  | "InsertUnion" -> prop_Q_InsertUnion
  | "DeleteUnion" -> prop_Q_DeleteUnion
  | "DeleteInsert" -> prop_Q_DeleteInsert
  | "DeleteDelete" -> prop_Q_DeleteDelete
  | "UnionDeleteInsert" -> prop_Q_UnionDeleteInsert
  | "UnionUnionIdem" -> prop_Q_UnionUnionIdem
  | "UnionUnionAssoc" -> prop_Q_UnionUnionAssoc
  | _ -> raise (Invalid_argument ("Unknown property: " ^ name))

let crowbar_property name =
  match name with
  | "InsertValid" -> prop_C_InsertValid
  | "DeleteValid" -> prop_C_DeleteValid
  | "UnionValid" -> prop_C_UnionValid
  | "InsertPost" -> prop_C_InsertPost
  | "DeletePost" -> prop_C_DeletePost
  | "UnionPost" -> prop_C_UnionPost
  | "InsertModel" -> prop_C_InsertModel
  | "DeleteModel" -> prop_C_DeleteModel
  | "UnionModel" -> prop_C_UnionModel
  | "InsertInsert" -> prop_C_InsertInsert
  | "InsertDelete" -> prop_C_InsertDelete
  | "InsertUnion" -> prop_C_InsertUnion
  | "DeleteUnion" -> prop_C_DeleteUnion
  | "DeleteInsert" -> prop_C_DeleteInsert
  | "DeleteDelete" -> prop_C_DeleteDelete
  | "UnionDeleteInsert" -> prop_C_UnionDeleteInsert
  | "UnionUnionIdem" -> prop_C_UnionUnionIdem
  | "UnionUnionAssoc" -> prop_C_UnionUnionAssoc
  | _ -> raise (Invalid_argument ("Unknown property: " ^ name))

let qcheck_generator name =
  match name with
  | "typebased" -> gen_Q_TypeBased
  | "bespoke" -> gen_Q_Bespoke
  | _ -> raise (Invalid_argument ("Unknown generator: " ^ name))

let crowbar_generator name =
  match name with
  | "typebased" -> gen_C_TypeBased
  | "bespoke" -> gen_C_Bespoke
  | _ -> raise (Invalid_argument ("Unknown generator: " ^ name))

(** Command line setup *)

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
  | "crowbar" ->
      (* have to do this because crowbar reads command lines args *)
      Random.self_init ();
      Sys.argv.(1) <- "--repeat=10000000";
      Sys.argv.(2) <- Printf.sprintf "--seed=%d" (Random.int 1000000);
      Runner_crowbar.run
        (crowbar_property property)
        (crowbar_generator generator)
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

open Cmdliner
open Sampler_qcheck
open Rbt.Gen_typebased_qcheck
open Rbt.Gen_bespoke_qcheck

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

(** Command line setup *)

let main property strategy count seed =
  (* Your logic here: select property and generator by name *)
  let framework, generator =
    match String.split_on_char ':' strategy with
    | [ framework; generator ] -> (framework, generator)
    | _ -> failwith "Strategy must be in the form FRAMEWORK:GENERATOR"
  in
  match framework |> String.lowercase_ascii with
  | "qcheck" ->
      (qcheck_property property) ~gen:(qcheck_generator generator) ~count ~seed
  | _ -> failwith "sampling is only supported for qcheck framework"

(** *)

(** Cmdliner stuff *)

(** | *)

(** v *)

let _ = Random.self_init ()

let property_arg =
  let doc = "Name of the property test to run." in
  Arg.(
    required
    & opt (some string) None
    & info [ "property" ] ~docv:"PROPERTY" ~doc)

let generator_arg =
  let doc = "Name of the strategy to use." in
  Arg.(
    required
    & opt (some string) None
    & info [ "strategy" ] ~docv:"FRAMEWORK:GENERATOR" ~doc)

let count_arg =
  let doc = "Number of samples to generate." in
  Arg.(value & opt int 10 & info [ "count" ] ~docv:"COUNT" ~doc)

let seed_arg =
  let doc = "Random seed for the generator." in
  Arg.(value & opt int (Random.int 1000000) & info [ "seed" ] ~docv:"SEED" ~doc)

let term =
  Term.(const main $ property_arg $ generator_arg $ count_arg $ seed_arg)

let () = Cmd.(exit @@ eval (v (info "BST") term))

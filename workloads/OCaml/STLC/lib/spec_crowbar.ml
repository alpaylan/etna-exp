open Crowbar
open Impl
open Spec

let int = int16
let start_time : float option ref = ref None
let end_time : float option ref = ref None
let discards : int ref = ref 0
let generated : int ref = ref 0

(** tapped versions of crowbar's stdlib*)
let add_test ~name gen f =
  add_test ~name gen (fun x ->
      if !start_time = None then start_time := Some (Unix.gettimeofday ());
      incr generated;
      f x)

let check b =
  if not b then end_time := Some (Unix.gettimeofday ());
  check b

let guard b =
  if not b then incr discards;
  guard b

(** Actual properties *)

let prop_C_InsertValid gen =
  add_test ~name:"C_InsertValid" [ gen ] (fun e ->
      guard (typechecks e);
      check (Option.value
        (mt e >>= fun t -> Some (mtypeCheck (pstep e) t))
        ~default:true))

let prop_C_DeleteValid gen =
  add_test ~name:"C_DeleteValid" [ gen ] (fun e ->
      guard (typechecks e);
      check (Option.value
        (mt e >>= fun t -> Some (mtypeCheck (multistep 40 pstep e) t))
        ~default:true))

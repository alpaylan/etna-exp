open Impl
open Crowbar

let ( >>= ) = dynamic_bind

let ( --- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

(** `oneofl xs` constructs a generator by randomly choosing an element of xs *)
let oneofl (xs : 'a list) : 'a gen =
  range (List.length xs) >>= fun n -> const (List.nth xs n)

let typegen : typ gen =
  let t_fun t t' = TFun (t, t') in
  fix (fun typeGen -> choose [ const TBool; map [ typeGen; typeGen ] t_fun ])

let genExactExpr (ctx : ctx) (t : typ) : expr gen =
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let rec _genOne ctx t =
    lazy
      (match (ctx, t) with
      | _, TBool -> map [ bool ] e_bool
      | ctx, TFun (t1, t2) -> map [ unlazy (_genOne (t1 :: ctx) t2) ] (e_abs t1))
  in
  let genOne ctx t = unlazy (_genOne ctx t) in
  let genVar (ctx : ctx) t =
    let vars =
      List.filter (fun i -> List.nth ctx i = t) (0 --- (List.length ctx - 1))
    in
    if List.length vars = 0 then [] else [ map [ oneofl vars ] e_var ]
  in
  let rec go ctx t =
    lazy
      (let genAbs ctx t1 t2 = map [ unlazy (go (t1 :: ctx) t2) ] (e_abs t1) in
       let genApp ctx t =
         typegen >>= fun t' ->
         unlazy (go ctx (TFun (t', t))) >>= fun e1 ->
         unlazy (go ctx t') >>= fun e2 -> const (App (e1, e2))
       in
       let absGen =
         match t with TFun (t1, t2) -> [ genAbs ctx t1 t2 ] | _ -> []
       in
       choose ([ genOne ctx t ] @ [ genApp ctx t ] @ absGen @ genVar ctx t))
  in
  unlazy (go ctx t)

let crowbarBespoke = typegen >>= genExactExpr []
let crowbar_bespoke = with_printer Display.format_expr crowbarBespoke

open Impl

let ( --- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let typGen =
  let tfun a b = TFun (a, b) in
  let open QCheck2.Gen in
  let rec _typGen n =
    if n <= 0 then return TBool
    else oneof [ return TBool; map2 tfun (_typGen (n / 2)) (_typGen (n / 2)) ]
  in
  sized _typGen

let genExactExpr (ctx : ctx) (t : typ) : expr QCheck2.Gen.t =
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let open QCheck2.Gen in
  let rec genOne ctx t =
    match (ctx, t) with
    | _, TBool -> e_bool <$> bool
    | ctx, TFun (t1, t2) -> e_abs t1 <$> genOne (t1 :: ctx) t2
  in
  let genVar (ctx : ctx) t =
    let open List in
    let vars = filter (fun i -> nth ctx i = t) (0 --- (length ctx - 1)) in
    if length vars = 0 then [] else [ e_var <$> oneofl vars ]
  in
  let rec go n ctx t =
    let genAbs ctx t1 t2 = e_abs t1 <$> go (n - 1) (t1 :: ctx) t2 in
    let genApp ctx t =
      typGen >>= fun t' ->
      go (n / 2) ctx (TFun (t', t)) >>= fun e1 ->
      go (n / 2) ctx t' >>= fun e2 -> return (App (e1, e2))
    in

    if n <= 0 then oneof (genOne ctx t :: genVar ctx t)
    else
      let absGen =
        match t with TFun (t1, t2) -> [ genAbs ctx t1 t2 ] | _ -> []
      in
      oneof ([ genOne ctx t ] @ [ genApp ctx t ] @ absGen @ genVar ctx t)
  in
  sized (fun n -> go n ctx t)

let gen_Q_Bespoke =
  let open QCheck2.Gen in
  let* typ = typGen in
  genExactExpr [] typ >>= fun expr ->
  return expr
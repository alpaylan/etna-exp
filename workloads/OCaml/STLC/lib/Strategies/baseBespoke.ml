open Impl


let genExactExpr (ctx : ctx) (t : typ) : expr Base_quickcheck.Generator.t =
  let open Base_quickcheck.Generator in
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let rec genOne ctx t =
    match (ctx, t) with
    | _, TBool -> bool >>| e_bool
    | ctx, TFun (t1, t2) -> genOne (t1 :: ctx) t2 >>| e_abs t1
  in
  let genVar ctx t =
    let vars =
      Core.List.filter
        (Core.List.range ~start:`inclusive ~stop:`exclusive 0
           (Core.List.length ctx))
        ~f:(fun i -> Core.List.nth_exn ctx i = t)
    in
    if Core.List.length vars = 0 then [] else [ of_list vars >>| e_var ]
  in
  let rec go ctx t =
    let genAbs ctx t1 t2 =
      size >>= fun n -> with_size ~size:(n - 1) (go (t1 :: ctx) t2) >>| e_abs t1
    in
    let genApp ctx t =
      quickcheck_generator_typ >>= fun t' ->
      size >>= fun n ->
      with_size ~size:(n - 1) (go ctx (TFun (t', t))) >>= fun e1 ->
      with_size ~size:(n - 1) (go ctx t') >>= fun e2 -> return (App (e1, e2))
    in

    size >>= fun n ->
    if n <= 0 then union (genOne ctx t :: genVar ctx t)
    else
      let absGen =
        match t with TFun (t1, t2) -> [ genAbs ctx t1 t2 ] | _ -> []
      in
      union ([ genOne ctx t ] @ [ genApp ctx t ] @ absGen @ genVar ctx t)
  in
  go ctx t

module BaseBespoke : Base_quickcheck.Test.S with type t = expr = struct
  type t = expr [@@deriving sexp, quickcheck]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    quickcheck_generator_typ >>= genExactExpr []
end

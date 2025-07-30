open Impl

let gen_Q_TypeBased =
  let open QCheck2.Gen in
  let typeGen =
    let rec _typeGen n =
      if n <= 0 then return TBool
      else
        oneof
          [
            map2
              (fun t1 t2 -> TFun (t1, t2))
              (_typeGen (n / 2))
              (_typeGen (n / 2));
            return TBool;
          ]
    in
    sized _typeGen
  in
  let rec _exprGen n =
    if n <= 0 then
      oneof [ map (fun n -> Var n) nat; map (fun b -> Bool b) bool ]
    else
      oneof
        [
          map2 (fun t e -> Abs (t, e)) typeGen (_exprGen (n / 2));
          map2 (fun e1 e2 -> App (e1, e2)) (_exprGen (n / 2)) (_exprGen (n / 2));
          map (fun n -> Var n) nat;
          map (fun b -> Bool b) bool;
        ]
  in
  sized (fun n -> _exprGen (n / 10))
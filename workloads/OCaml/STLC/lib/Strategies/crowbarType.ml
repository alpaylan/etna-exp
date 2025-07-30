open Impl
open Crowbar

let typegen : typ gen =
  let t_fun t t' = TFun (t, t') in
  fix (fun typeGen -> choose [ const TBool; map [ typeGen; typeGen ] t_fun ])

let crowbarType : expr gen =
  let e_var v = Var v in
  let e_bool b = Bool b in
  let e_abs t e = Abs (t, e) in
  let e_app e e' = App (e, e') in
  fix (fun exprGen ->
      choose
        [
          map [ uint8 ] e_var;
          map [ bool ] e_bool;
          map [ typegen; exprGen ] e_abs;
          map [ exprGen; exprGen ] e_app;
        ])


let crowbar_type = with_printer Display.format_expr crowbarType

open Impl

let rec string_of_expr (e : expr) : string =
  let rec string_of_typ (t : typ) : string =
    match t with
    | TBool -> "TBool"
    | TFun (t1, t2) ->
        "TFun (" ^ string_of_typ t1 ^ ", " ^ string_of_typ t2 ^ ")"
  in
  match e with
  | Bool b -> "Bool " ^ string_of_bool b
  | Var i -> "Var " ^ string_of_int i
  | Abs (t, e') -> "Abs (" ^ string_of_typ t ^ ", " ^ string_of_expr e' ^ ")"
  | App (e1, e2) -> "App (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

let rec format_expr fmt e =
  let rec format_typ fmt t =
    match t with
    | TBool -> Format.fprintf fmt "TBool"
    | TFun (t, t') -> Format.fprintf fmt "TFun (%a, %a)" format_typ t format_typ t'
  in
  match e with
  | Bool b -> Format.fprintf fmt "Bool %b" b
  | Var i -> Format.fprintf fmt "Var %i" i
  | Abs (t, e') -> Format.fprintf fmt "Abs (%a, %a)" format_typ t format_expr e'
  | App (e1, e2) -> Format.fprintf fmt "App (%a, %a)" format_expr e1 format_expr e2

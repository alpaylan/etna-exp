open Impl
open Util.Runner

let mt (e : expr) : typ option = getTyp [] e
let typechecks (e : expr) : bool = Option.is_some (mt e)

let mtypeCheck (e : expr option) (t : typ) : bool =
  match e with Some e' -> typeCheck [] e' t | None -> true


let prop_SinglePreserve (e : expr) : test =
  typechecks e
  ->> Option.value
        (mt e >>= fun t -> Some (mtypeCheck (pstep e) t))
        ~default:true

let prop_MultiPreserve (e : expr) : test =
  typechecks e
  ->> Option.value
        (mt e >>= fun t -> Some (mtypeCheck (multistep 40 pstep e) t))
        ~default:true

let rec sizeSTLC (e : expr) : int =
  match e with
  | Abs (_, e) -> 1 + sizeSTLC e
  | App (e1, e2) -> 1 + sizeSTLC e1 + sizeSTLC e2
  | _ -> 1

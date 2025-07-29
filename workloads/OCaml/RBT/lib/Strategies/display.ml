open Impl

let format_color fmt c =
  match c with R -> Format.fprintf fmt "R" | B -> Format.fprintf fmt "B"

let rec format_tree fmt tree =
  match tree with
  | E -> Format.fprintf fmt "E"
  | T (color, left, key, value, right) ->
      Format.fprintf fmt "(T %a %a %d %d %a)" format_color color format_tree
        left key value format_tree right

let rec string_of_tree t =
  match t with
  | E -> "Empty"
  | T (c, l, k, v, r) ->
      let cs = if c = R then "R" else "B" in
      "Tree (" ^ cs ^ "," ^ string_of_tree l ^ "," ^ string_of_int k ^ ","
      ^ string_of_int v ^ "," ^ string_of_tree r ^ ")"
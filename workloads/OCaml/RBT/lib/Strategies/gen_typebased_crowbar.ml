open Impl
open Crowbar

let _cbtype : rbt gen =
  let t c l k v r = T (c, l, k, v, r) in
  fix (fun cbtype ->
      choose
        [
          const E;
          map [ choose [ const R; const B ]; cbtype; int8; int8; cbtype ] t;
        ])

let crowbar_type = with_printer Display.format_tree _cbtype

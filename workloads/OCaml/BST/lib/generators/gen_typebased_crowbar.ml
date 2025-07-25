open Crowbar
open Impl

let gen_C_TypeBased : t gen =
  fix (fun cbtype ->
      choose
        [
          const E;
          map [ cbtype; cbtype; int8; int8 ] (fun l r k v -> T (l, k, v, r));
        ])

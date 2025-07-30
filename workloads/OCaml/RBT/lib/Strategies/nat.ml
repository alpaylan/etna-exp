module Nat = struct
  include Core.Int

  type t = Core.Int.t [@@deriving sexp, quickcheck]
  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    int >>= fun i -> return (i % 128)
end

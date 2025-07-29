open Impl

module BaseType : Base_quickcheck.Test.S with type t = expr = struct
  type t = expr [@@deriving sexp, quickcheck]
end

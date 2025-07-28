open Impl

let gen_Q_TypeBased =
  QCheck2.Gen.(
    sized
    @@ fix (fun gen n ->
           match n with
           | 0 -> return E
           | _ ->
               let* c = frequency [ (1, return R); (1, return B) ] in
               let* k = small_int in
               let* v = small_int in
               let* l = gen (n / 2) in
               let* r = gen (n / 2) in
               return (T (c, l, k, v, r))))


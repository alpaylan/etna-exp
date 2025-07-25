open Impl

let gen_Q_TypeBased =
  QCheck2.Gen.(
    sized
    @@ fix (fun gen n ->
           match n with
           | 0 -> return E
           | _ ->
               let* k = small_int in
               let* v = small_int in
               let* l = gen (n / 2) in
               let* r = gen (n / 2) in
               return (T (l, k, v, r))))

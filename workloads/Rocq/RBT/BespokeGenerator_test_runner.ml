
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type comparison =
| Eq
| Lt
| Gt

type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

(** val revapp : uint -> uint -> uint **)

let rec revapp d d' =
  match d with
  | Nil -> d'
  | D0 d0 -> revapp d0 (D0 d')
  | D1 d0 -> revapp d0 (D1 d')
  | D2 d0 -> revapp d0 (D2 d')
  | D3 d0 -> revapp d0 (D3 d')
  | D4 d0 -> revapp d0 (D4 d')
  | D5 d0 -> revapp d0 (D5 d')
  | D6 d0 -> revapp d0 (D6 d')
  | D7 d0 -> revapp d0 (D7 d')
  | D8 d0 -> revapp d0 (D8 d')
  | D9 d0 -> revapp d0 (D9 d')

(** val rev : uint -> uint **)

let rev d =
  revapp d Nil

module Little =
 struct
  (** val succ : uint -> uint **)

  let rec succ = function
  | Nil -> D1 Nil
  | D0 d0 -> D1 d0
  | D1 d0 -> D2 d0
  | D2 d0 -> D3 d0
  | D3 d0 -> D4 d0
  | D4 d0 -> D5 d0
  | D5 d0 -> D6 d0
  | D6 d0 -> D7 d0
  | D7 d0 -> D8 d0
  | D8 d0 -> D9 d0
  | D9 d0 -> D0 (succ d0)

  (** val double : uint -> uint **)

  let rec double = function
  | Nil -> Nil
  | D0 d0 -> D0 (double d0)
  | D1 d0 -> D2 (double d0)
  | D2 d0 -> D4 (double d0)
  | D3 d0 -> D6 (double d0)
  | D4 d0 -> D8 (double d0)
  | D5 d0 -> D0 (succ_double d0)
  | D6 d0 -> D2 (succ_double d0)
  | D7 d0 -> D4 (succ_double d0)
  | D8 d0 -> D6 (succ_double d0)
  | D9 d0 -> D8 (succ_double d0)

  (** val succ_double : uint -> uint **)

  and succ_double = function
  | Nil -> D1 Nil
  | D0 d0 -> D1 (double d0)
  | D1 d0 -> D3 (double d0)
  | D2 d0 -> D5 (double d0)
  | D3 d0 -> D7 (double d0)
  | D4 d0 -> D9 (double d0)
  | D5 d0 -> D1 (succ_double d0)
  | D6 d0 -> D3 (succ_double d0)
  | D7 d0 -> D5 (succ_double d0)
  | D8 d0 -> D7 (succ_double d0)
  | D9 d0 -> D9 (succ_double d0)
 end

module Coq__1 = struct
 (** val add : int -> int -> int **)

 let rec add = (+)
end
include Coq__1

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Stdlib.max 0 (n-m)



(** val internal_eq_rew_r_dep : 'a1 -> 'a1 -> 'a2 -> 'a2 **)

let internal_eq_rew_r_dep _ _ hC =
  hC

module type EqLtLe =
 sig
  type t
 end

module MakeOrderTac =
 functor (O:EqLtLe) ->
 functor (P:sig
 end) ->
 struct
 end

module Nat =
 struct
  (** val sub : int -> int -> int **)

  let rec sub = (-)

  (** val eqb : int -> int -> bool **)

  let rec eqb = (=)

  (** val leb : int -> int -> bool **)

  let rec leb = (<=)

  (** val to_little_uint : int -> uint -> uint **)

  let rec to_little_uint n0 acc =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> acc)
      (fun n1 -> to_little_uint n1 (Little.succ acc))
      n0

  (** val to_uint : int -> uint **)

  let to_uint n0 =
    rev (to_little_uint n0 (D0 Nil))

  (** val divmod : int -> int -> int -> int -> int * int **)

  let rec divmod x y q u =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> (q, u))
      (fun x' ->
      (fun fO fS n -> if n=0 then fO () else fS (n-1))
        (fun _ -> divmod x' y (Stdlib.Int.succ q) y)
        (fun u' -> divmod x' y q u')
        u)
      x

  (** val div : int -> int -> int **)

  let div = (fun x -> function 0 -> 0 | y -> x / y)

  (** val modulo : int -> int -> int **)

  let modulo = (fun x -> function 0 -> x | y -> x mod y)
 end

module Pos =
 struct
  (** val succ : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec succ = Big_int_Z.succ_big_int

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec add = Big_int_Z.add_big_int

  (** val add_carry :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  and add_carry x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (add_carry p q))
        (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
        (fun _ ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (add p q))
        (fun _ -> Big_int_Z.mult_int_big_int 2 (succ p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (succ q))
        (fun q -> Big_int_Z.mult_int_big_int 2 (succ q))
        (fun _ ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)
        y)
      x

  (** val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 p))
      (fun p ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (pred_double p))
      (fun _ -> Big_int_Z.unit_big_int)
      x

  (** val mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec mul = Big_int_Z.mult_big_int

  (** val compare_cont :
      comparison -> Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let rec compare_cont = (fun c x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then c else if s < 0 then Lt else Gt)

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eqb p q =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        (fun _ -> false)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        q)
      p

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> Big_int_Z.big_int -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 -> op a (iter_op op p0 (op a a)))
      (fun p0 -> iter_op op p0 (op a a))
      (fun _ -> a)
      p

  (** val to_nat : Big_int_Z.big_int -> int **)

  let to_nat x =
    iter_op Coq__1.add x (Stdlib.Int.succ 0)

  (** val of_succ_nat : int -> Big_int_Z.big_int **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun x -> succ (of_succ_nat x))
      n0

  (** val to_little_uint : Big_int_Z.big_int -> uint **)

  let rec to_little_uint p =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 -> Little.succ_double (to_little_uint p0))
      (fun p0 -> Little.double (to_little_uint p0))
      (fun _ -> D1 Nil)
      p

  (** val to_uint : Big_int_Z.big_int -> uint **)

  let to_uint p =
    rev (to_little_uint p)

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eq_dec p x0 =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        (fun _ -> false)
        x0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        x0)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        x0)
      p
 end

module N =
 struct
  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let add = Big_int_Z.add_big_int

  (** val mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let mul = Big_int_Z.mult_big_int

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)
 end

(** val n_of_digits : bool list -> Big_int_Z.big_int **)

let rec n_of_digits = function
| [] -> Big_int_Z.zero_big_int
| b :: l' ->
  N.add (if b then Big_int_Z.unit_big_int else Big_int_Z.zero_big_int)
    (N.mul (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
      (n_of_digits l'))

(** val n_of_ascii : char -> Big_int_Z.big_int **)

let n_of_ascii a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun a0 a1 a2 a3 a4 a5 a6 a7 ->
    n_of_digits
      (a0 :: (a1 :: (a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: [])))))))))
    a

(** val rev0 : 'a1 list -> 'a1 list **)

let rec rev0 = function
| [] -> []
| x :: l' -> app (rev0 l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t0 -> (f a) :: (map f t0)

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t0 -> fold_left f t0 (f a0 b)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t0 -> f b (fold_right f a0 t0)

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec filter f = function
| [] -> []
| x :: l0 -> if f x then x :: (filter f l0) else filter f l0

(** val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list **)

let rec combine l l' =
  match l with
  | [] -> []
  | x :: tl ->
    (match l' with
     | [] -> []
     | y :: tl' -> (x, y) :: (combine tl tl'))

module Z =
 struct
  (** val double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> (Big_int_Z.mult_int_big_int 2 p))
      (fun p -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2 p))
      x

  (** val succ_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let succ_double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun p ->
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      p))
      (fun p -> Big_int_Z.minus_big_int (Pos.pred_double p))
      x

  (** val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let pred_double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.minus_big_int Big_int_Z.unit_big_int)
      (fun p -> (Pos.pred_double p))
      (fun p -> Big_int_Z.minus_big_int
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) p))
      x

  (** val pos_sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> double (pos_sub p q))
        (fun q -> succ_double (pos_sub p q))
        (fun _ -> (Big_int_Z.mult_int_big_int 2 p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> pred_double (pos_sub p q))
        (fun q -> double (pos_sub p q))
        (fun _ -> (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2
        q))
        (fun q -> Big_int_Z.minus_big_int (Pos.pred_double q))
        (fun _ -> Big_int_Z.zero_big_int)
        y)
      x

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let add = Big_int_Z.add_big_int

  (** val opp : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let opp = Big_int_Z.minus_big_int

  (** val succ : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let succ = Big_int_Z.succ_big_int

  (** val sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sub = Big_int_Z.sub_big_int

  (** val mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let mul = Big_int_Z.mult_big_int

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val gtb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let gtb x y =
    match compare x y with
    | Gt -> true
    | _ -> false

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eqb = Big_int_Z.eq_big_int

  (** val max :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let max = Big_int_Z.max_big_int

  (** val to_nat : Big_int_Z.big_int -> int **)

  let to_nat z0 =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> 0)
      (fun p -> Pos.to_nat p)
      (fun _ -> 0)
      z0

  (** val of_nat : int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n1 -> (Pos.of_succ_nat n1))
      n0

  (** val to_int :
      Big_int_Z.big_int ->
      ((Obj.t -> Obj.t) -> (Obj.t -> Obj.t) -> Obj.t) (* Decimal.int *) **)

  let to_int n0 =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> (fun x pos _ -> pos (Obj.magic x)) (D0 Nil))
      (fun p -> (fun x pos _ -> pos (Obj.magic x)) (Pos.to_uint p))
      (fun p -> (fun y _ neg -> neg (Obj.magic y)) (Pos.to_uint p))
      n0

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eq_dec = Big_int_Z.eq_big_int
 end

(** val z_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_lt_ge_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_ge_dec =
  z_lt_dec

(** val z_lt_le_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_le_dec =
  z_lt_ge_dec

(** val string_dec : char list -> char list -> bool **)

let rec string_dec s x =
  match s with
  | [] -> (match x with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x with
     | [] -> false
     | a0::s1 -> if (=) a a0 then string_dec s0 s1 else false)

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val newline : char list **)

let newline =
  '\n'::[]

type 'a show =
  'a -> char list
  (* singleton inductive, whose constructor was Build_Show *)

(** val show_nat : int -> char list **)

let show_nat = (fun i ->
  let s = string_of_int i in
  let rec copy acc i =
    if i < 0 then acc else copy (s.[i] :: acc) (i-1)
  in copy [] (String.length s - 1))

(** val show_Z : Big_int_Z.big_int -> char list **)

let show_Z = (fun i ->
  let s = Big_int_Z.string_of_big_int i in
  let rec copy acc i =
    if i < 0 then acc else copy (s.[i] :: acc) (i-1)
  in copy [] (String.length s - 1))

(** val showNat : int show **)

let showNat =
  show_nat

(** val showZ : Big_int_Z.big_int show **)

let showZ =
  show_Z

(** val nl : char list **)

let nl =
  '\n'::[]

module ShowFunctions =
 struct
  (** val prepend : 'a1 -> 'a1 list -> 'a1 list **)

  let rec prepend a = function
  | [] -> []
  | h :: t0 -> a :: (h :: (prepend a t0))

  (** val intersperse : 'a1 -> 'a1 list -> 'a1 list **)

  let intersperse a = function
  | [] -> []
  | h :: t0 -> h :: (prepend a t0)

  (** val string_concat : char list list -> char list **)

  let string_concat l =
    fold_left append l []
 end

(** val trace : char list -> 'a1 -> 'a1 **)

let trace = (fun l -> print_string (
   let s = Bytes.create (List.length l) in
   let rec copy i = function
    | [] -> s
    | c :: l -> Bytes.set s i c; copy (i+1) l
   in Bytes.to_string (copy 0 l)); flush stdout; fun y -> y)

type 'f functor0 =
  __ -> __ -> (__ -> __) -> 'f -> 'f
  (* singleton inductive, whose constructor was Build_Functor *)

(** val fmap : 'a1 functor0 -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let fmap functor1 x x0 =
  Obj.magic functor1 __ __ x x0

type 'm monad = { ret : (__ -> __ -> 'm);
                  bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

(** val ret : 'a1 monad -> 'a2 -> 'a1 **)

let ret monad0 x =
  Obj.magic monad0.ret __ x

(** val bind : 'a1 monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad0 x x0 =
  Obj.magic monad0.bind __ __ x x0

(** val liftM : 'a1 monad -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let liftM m f x =
  bind m x (fun x0 -> ret m (f x0))

(** val functor_Monad : 'a1 monad -> 'a1 functor0 **)

let functor_Monad m _ _ =
  liftM m

type decidable = bool

type 'a lazyList =
| Lnil
| Lcons of 'a * (unit -> 'a lazyList)

(** val lazy_seq : ('a1 -> 'a1) -> 'a1 -> int -> 'a1 lazyList **)

let rec lazy_seq s lo len =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> Lnil)
    (fun len' -> Lcons (lo, (fun _ -> lazy_seq s (s lo) len')))
    len

type randomSeed = Random.State.t

(** val newRandomSeed : randomSeed **)

let newRandomSeed = (Random.State.make_self_init ())

(** val randomSplit : randomSeed -> randomSeed * randomSeed **)

let randomSplit = (fun x -> (x,x))

(** val randomRNat : (int * int) -> randomSeed -> int * randomSeed **)

let randomRNat = (fun (x,y) r -> if y < x then failwith (Printf.sprintf "choose called with unordered arguments (%d, %d)" x y) else  (x + (Random.State.full_int r (y - x + 1)), r))

(** val randomRInt :
    (Big_int_Z.big_int * Big_int_Z.big_int) -> randomSeed ->
    Big_int_Z.big_int * randomSeed **)

let randomRInt = (fun (x,y) r ->
   if Big_int_Z.lt_big_int y x
   then failwith (Printf.sprintf "choose called with unordered arguments (%s, %s)" (Big_int_Z.string_of_big_int x) (Big_int_Z.string_of_big_int y))
   else
    let range_Z = Big_int_Z.succ_big_int (Big_int_Z.sub_big_int y x) in
    let range_int = Big_int_Z.int_of_big_int range_Z in
    (Big_int_Z.add_big_int x (Big_int_Z.big_int_of_int (Random.State.int r range_int)), r))

type 'a choosableFromInterval = { randomR : (('a * 'a) -> randomSeed ->
                                            'a * randomSeed);
                                  enumR : (('a * 'a) -> 'a lazyList) }

(** val enumRNat : (int * int) -> int lazyList **)

let enumRNat p =
  lazy_seq (fun x -> Stdlib.Int.succ x) (fst p) (Stdlib.Int.succ
    (sub (snd p) (fst p)))

(** val chooseNat : int choosableFromInterval **)

let chooseNat =
  { randomR = randomRNat; enumR = enumRNat }

(** val enumRZ :
    (Big_int_Z.big_int * Big_int_Z.big_int) -> Big_int_Z.big_int lazyList **)

let enumRZ p =
  lazy_seq Z.succ (fst p) (Stdlib.Int.succ (Z.to_nat (Z.sub (snd p) (fst p))))

(** val chooseZ : Big_int_Z.big_int choosableFromInterval **)

let chooseZ =
  { randomR = randomRInt; enumR = enumRZ }

type 'g producer = { super : 'g monad; sample : (__ -> 'g -> __ list);
                     sized : (__ -> (int -> 'g) -> 'g);
                     resize : (__ -> int -> 'g -> 'g);
                     choose : (__ -> __ -> __ choosableFromInterval ->
                              (__ * __) -> 'g);
                     bindPf : (__ -> __ -> 'g -> (__ -> __ -> 'g) -> 'g) }

(** val sized : 'a1 producer -> (int -> 'a1) -> 'a1 **)

let sized producer0 x =
  producer0.sized __ x

(** val choose :
    'a1 producer -> 'a2 choosableFromInterval -> ('a2 * 'a2) -> 'a1 **)

let choose producer0 h x =
  Obj.magic producer0.choose __ __ h x

(** val force : 'a1 Lazy.t -> 'a1 **)

let force = Lazy.force

type 'a rose =
| MkRose of 'a * 'a rose list Lazy.t

(** val returnRose : 'a1 -> 'a1 rose **)

let returnRose x =
  MkRose (x, (lazy []))

(** val fmapRose : ('a1 -> 'a2) -> 'a1 rose -> 'a2 rose **)

let rec fmapRose f = function
| MkRose (x, rs) -> MkRose ((f x), (lazy (map (fmapRose f) (force rs))))

type 'a genType =
  int -> randomSeed -> 'a
  (* singleton inductive, whose constructor was MkGen *)

type 'a g = 'a genType

(** val run : 'a1 g -> int -> randomSeed -> 'a1 **)

let run g0 =
  g0

(** val returnGen : 'a1 -> 'a1 g **)

let returnGen x _ _ =
  x

(** val bindGen : 'a1 g -> ('a1 -> 'a2 g) -> 'a2 g **)

let bindGen g0 k n0 r =
  let (r1, r2) = randomSplit r in run (k (run g0 n0 r1)) n0 r2

(** val monadGen : __ g monad **)

let monadGen =
  { ret = (fun _ -> returnGen); bind = (fun _ _ -> bindGen) }

(** val createRange : int -> int list -> int list **)

let rec createRange n0 acc =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> rev0 (0 :: acc))
    (fun n' -> createRange n' (n0 :: acc))
    n0

(** val rnds : randomSeed -> int -> randomSeed list **)

let rec rnds s n' =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n'' -> let (s1, s2) = randomSplit s in s1 :: (rnds s2 n''))
    n'

(** val sampleGen : 'a1 g -> 'a1 list **)

let sampleGen g0 =
  let l =
    combine
      (rnds newRandomSeed (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ 0)))))))))))))))))))))
      (createRange (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
        (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))) [])
  in
  map (fun p -> let (r, n0) = p in g0 n0 r) l

(** val sizedGen : (int -> 'a1 g) -> 'a1 g **)

let sizedGen f n0 r =
  run (f n0) n0 r

(** val resizeGen : int -> 'a1 g -> 'a1 g **)

let resizeGen n0 g0 _ =
  g0 n0

(** val chooseGen : 'a1 choosableFromInterval -> ('a1 * 'a1) -> 'a1 g **)

let chooseGen h range _ r =
  fst (h.randomR range r)

(** val producerGen : __ g producer **)

let producerGen =
  { super = monadGen; sample = (fun _ -> sampleGen); sized = (fun _ ->
    sizedGen); resize = (fun _ -> resizeGen); choose = (fun _ _ ->
    chooseGen); bindPf = (fun _ _ g0 k n0 r ->
    let (r1, r2) = randomSplit r in run (k (run g0 n0 r1) __) n0 r2) }

type 'x compare0 =
| LT
| EQ
| GT

module type OrderedType =
 sig
  type t

  val compare : t -> t -> t compare0

  val eq_dec : t -> t -> bool
 end

module OrderedTypeFacts =
 functor (O:OrderedType) ->
 struct
  module TO =
   struct
    type t = O.t
   end

  module IsTO =
   struct
   end

  module OrderTac = MakeOrderTac(TO)(IsTO)

  (** val eq_dec : O.t -> O.t -> bool **)

  let eq_dec =
    O.eq_dec

  (** val lt_dec : O.t -> O.t -> bool **)

  let lt_dec x y =
    match O.compare x y with
    | LT -> true
    | _ -> false

  (** val eqb : O.t -> O.t -> bool **)

  let eqb x y =
    if eq_dec x y then true else false
 end

module KeyOrderedType =
 functor (O:OrderedType) ->
 struct
  module MO = OrderedTypeFacts(O)
 end

module AsciiOT =
 struct
  type t = char

  (** val compare : t -> t -> char compare0 **)

  let compare c d =
    let c0 = N.compare (n_of_ascii c) (n_of_ascii d) in
    (match c0 with
     | Eq -> EQ
     | Lt -> LT
     | Gt -> GT)
 end

module StringOT =
 struct
  type t = char list

  (** val eq_dec : char list -> char list -> bool **)

  let eq_dec =
    string_dec

  type coq_SOrdering =
  | SLT
  | SEQ
  | SGT

  (** val coq_SOrdering_rect : 'a1 -> 'a1 -> 'a1 -> coq_SOrdering -> 'a1 **)

  let coq_SOrdering_rect f f0 f1 = function
  | SLT -> f
  | SEQ -> f0
  | SGT -> f1

  (** val coq_SOrdering_rec : 'a1 -> 'a1 -> 'a1 -> coq_SOrdering -> 'a1 **)

  let coq_SOrdering_rec f f0 f1 = function
  | SLT -> f
  | SEQ -> f0
  | SGT -> f1

  (** val strcmp : char list -> char list -> coq_SOrdering **)

  let rec strcmp s1 s2 =
    match s1 with
    | [] -> (match s2 with
             | [] -> SEQ
             | _::_ -> SLT)
    | ch1::s1' ->
      (match s2 with
       | [] -> SGT
       | ch2::s2' ->
         (match AsciiOT.compare ch1 ch2 with
          | LT -> SLT
          | EQ -> strcmp s1' s2'
          | GT -> SGT))

  (** val compare : t -> t -> char list compare0 **)

  let rec compare s s2 =
    match s with
    | [] -> (match s2 with
             | [] -> EQ
             | _::_ -> LT)
    | a::s0 ->
      (match s2 with
       | [] -> GT
       | a0::s1 ->
         let c = AsciiOT.compare a a0 in
         (match c with
          | LT -> LT
          | EQ -> internal_eq_rew_r_dep a a0 (fun _ -> compare s0 s1) __
          | GT -> GT))
 end

module Raw =
 functor (X:OrderedType) ->
 struct
  module MX = OrderedTypeFacts(X)

  module PX = KeyOrderedType(X)

  type key = X.t

  type 'elt t = (X.t * 'elt) list

  (** val empty : 'a1 t **)

  let empty =
    []

  (** val is_empty : 'a1 t -> bool **)

  let is_empty = function
  | [] -> true
  | _ :: _ -> false

  (** val mem : key -> 'a1 t -> bool **)

  let rec mem k = function
  | [] -> false
  | p :: l ->
    let (k', _) = p in
    (match X.compare k k' with
     | LT -> false
     | EQ -> true
     | GT -> mem k l)

  (** val find : key -> 'a1 t -> 'a1 option **)

  let rec find k = function
  | [] -> None
  | p :: s' ->
    let (k', x) = p in
    (match X.compare k k' with
     | LT -> None
     | EQ -> Some x
     | GT -> find k s')

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec add k x s = match s with
  | [] -> (k, x) :: []
  | p :: l ->
    let (k', y) = p in
    (match X.compare k k' with
     | LT -> (k, x) :: s
     | EQ -> (k, x) :: l
     | GT -> (k', y) :: (add k x l))

  (** val remove : key -> 'a1 t -> 'a1 t **)

  let rec remove k s = match s with
  | [] -> []
  | p :: l ->
    let (k', x) = p in
    (match X.compare k k' with
     | LT -> s
     | EQ -> l
     | GT -> (k', x) :: (remove k l))

  (** val elements : 'a1 t -> 'a1 t **)

  let elements m =
    m

  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let rec fold f m acc =
    match m with
    | [] -> acc
    | p :: m' -> let (k, e) = p in fold f m' (f k e acc)

  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let rec equal cmp m m' =
    match m with
    | [] -> (match m' with
             | [] -> true
             | _ :: _ -> false)
    | p :: l ->
      let (x, e) = p in
      (match m' with
       | [] -> false
       | p0 :: l' ->
         let (x', e') = p0 in
         (match X.compare x x' with
          | EQ -> (&&) (cmp e e') (equal cmp l l')
          | _ -> false))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let rec map f = function
  | [] -> []
  | p :: m' -> let (k, e) = p in (k, (f e)) :: (map f m')

  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let rec mapi f = function
  | [] -> []
  | p :: m' -> let (k, e) = p in (k, (f k e)) :: (mapi f m')

  (** val option_cons :
      key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list **)

  let option_cons k o l =
    match o with
    | Some e -> (k, e) :: l
    | None -> l

  (** val map2_l :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)

  let rec map2_l f = function
  | [] -> []
  | p :: l -> let (k, e) = p in option_cons k (f (Some e) None) (map2_l f l)

  (** val map2_r :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)

  let rec map2_r f = function
  | [] -> []
  | p :: l' ->
    let (k, e') = p in option_cons k (f None (Some e')) (map2_r f l')

  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let rec map2 f m = match m with
  | [] -> map2_r f
  | p :: l ->
    let (k, e) = p in
    let rec map2_aux m' = match m' with
    | [] -> map2_l f m
    | p0 :: l' ->
      let (k', e') = p0 in
      (match X.compare k k' with
       | LT -> option_cons k (f (Some e) None) (map2 f l m')
       | EQ -> option_cons k (f (Some e) (Some e')) (map2 f l l')
       | GT -> option_cons k' (f None (Some e')) (map2_aux l'))
    in map2_aux

  (** val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t **)

  let rec combine m = match m with
  | [] -> map (fun e' -> (None, (Some e')))
  | p :: l ->
    let (k, e) = p in
    let rec combine_aux m' = match m' with
    | [] -> map (fun e0 -> ((Some e0), None)) m
    | p0 :: l' ->
      let (k', e') = p0 in
      (match X.compare k k' with
       | LT -> (k, ((Some e), None)) :: (combine l m')
       | EQ -> (k, ((Some e), (Some e'))) :: (combine l l')
       | GT -> (k', (None, (Some e'))) :: (combine_aux l'))
    in combine_aux

  (** val fold_right_pair :
      ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3 **)

  let fold_right_pair f l i =
    fold_right (fun p -> f (fst p) (snd p)) i l

  (** val map2_alt :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
      (key * 'a3) list **)

  let map2_alt f m m' =
    let m0 = combine m m' in
    let m1 = map (fun p -> f (fst p) (snd p)) m0 in
    fold_right_pair option_cons m1 []

  (** val at_least_one :
      'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option **)

  let at_least_one o o' =
    match o with
    | Some _ -> Some (o, o')
    | None -> (match o' with
               | Some _ -> Some (o, o')
               | None -> None)

  (** val at_least_one_then_f :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2 option ->
      'a3 option **)

  let at_least_one_then_f f o o' =
    match o with
    | Some _ -> f o o'
    | None -> (match o' with
               | Some _ -> f o o'
               | None -> None)
 end

module type Int =
 sig
  type t

  val i2z : t -> Big_int_Z.big_int

  val _0 : t

  val _1 : t

  val _2 : t

  val _3 : t

  val add : t -> t -> t

  val opp : t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val max : t -> t -> t

  val eqb : t -> t -> bool

  val ltb : t -> t -> bool

  val leb : t -> t -> bool

  val gt_le_dec : t -> t -> bool

  val ge_lt_dec : t -> t -> bool

  val eq_dec : t -> t -> bool
 end

module Z_as_Int =
 struct
  type t = Big_int_Z.big_int

  (** val _0 : Big_int_Z.big_int **)

  let _0 =
    Big_int_Z.zero_big_int

  (** val _1 : Big_int_Z.big_int **)

  let _1 =
    Big_int_Z.unit_big_int

  (** val _2 : Big_int_Z.big_int **)

  let _2 =
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)

  (** val _3 : Big_int_Z.big_int **)

  let _3 =
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int)

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let add =
    Z.add

  (** val opp : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let opp =
    Z.opp

  (** val sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sub =
    Z.sub

  (** val mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let mul =
    Z.mul

  (** val max :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let max =
    Z.max

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eqb =
    Z.eqb

  (** val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ltb =
    Z.ltb

  (** val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let leb =
    Z.leb

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eq_dec =
    Z.eq_dec

  (** val gt_le_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let gt_le_dec i j =
    let b = Z.ltb j i in if b then true else false

  (** val ge_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ge_lt_dec i j =
    let b = Z.ltb i j in if b then false else true

  (** val i2z : t -> Big_int_Z.big_int **)

  let i2z n0 =
    n0
 end

module Coq_Raw =
 functor (I:Int) ->
 functor (X:OrderedType) ->
 struct
  type key = X.t

  type 'elt tree =
  | Leaf
  | Node of 'elt tree * key * 'elt * 'elt tree * I.t

  (** val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2 **)

  let rec tree_rect f f0 = function
  | Leaf -> f
  | Node (t1, k, y, t2, t3) ->
    f0 t1 (tree_rect f f0 t1) k y t2 (tree_rect f f0 t2) t3

  (** val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2 **)

  let rec tree_rec f f0 = function
  | Leaf -> f
  | Node (t1, k, y, t2, t3) ->
    f0 t1 (tree_rec f f0 t1) k y t2 (tree_rec f f0 t2) t3

  (** val height : 'a1 tree -> I.t **)

  let height = function
  | Leaf -> I._0
  | Node (_, _, _, _, h) -> h

  (** val cardinal : 'a1 tree -> int **)

  let rec cardinal = function
  | Leaf -> 0
  | Node (l, _, _, r, _) -> Stdlib.Int.succ (add (cardinal l) (cardinal r))

  (** val empty : 'a1 tree **)

  let empty =
    Leaf

  (** val is_empty : 'a1 tree -> bool **)

  let is_empty = function
  | Leaf -> true
  | Node (_, _, _, _, _) -> false

  (** val mem : X.t -> 'a1 tree -> bool **)

  let rec mem x = function
  | Leaf -> false
  | Node (l, y, _, r, _) ->
    (match X.compare x y with
     | LT -> mem x l
     | EQ -> true
     | GT -> mem x r)

  (** val find : X.t -> 'a1 tree -> 'a1 option **)

  let rec find x = function
  | Leaf -> None
  | Node (l, y, d, r, _) ->
    (match X.compare x y with
     | LT -> find x l
     | EQ -> Some d
     | GT -> find x r)

  (** val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let create l x e r =
    Node (l, x, e, r, (I.add (I.max (height l) (height r)) I._1))

  (** val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let assert_false =
    create

  (** val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if I.gt_le_dec hl (I.add hr I._2)
    then (match l with
          | Leaf -> assert_false l x d r
          | Node (ll, lx, ld, lr, _) ->
            if I.ge_lt_dec (height ll) (height lr)
            then create ll lx ld (create lr x d r)
            else (match lr with
                  | Leaf -> assert_false l x d r
                  | Node (lrl, lrx, lrd, lrr, _) ->
                    create (create ll lx ld lrl) lrx lrd (create lrr x d r)))
    else if I.gt_le_dec hr (I.add hl I._2)
         then (match r with
               | Leaf -> assert_false l x d r
               | Node (rl, rx, rd, rr, _) ->
                 if I.ge_lt_dec (height rr) (height rl)
                 then create (create l x d rl) rx rd rr
                 else (match rl with
                       | Leaf -> assert_false l x d r
                       | Node (rll, rlx, rld, rlr, _) ->
                         create (create l x d rll) rlx rld
                           (create rlr rx rd rr)))
         else create l x d r

  (** val add : key -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let rec add x d = function
  | Leaf -> Node (Leaf, x, d, Leaf, I._1)
  | Node (l, y, d', r, h) ->
    (match X.compare x y with
     | LT -> bal (add x d l) y d' r
     | EQ -> Node (l, y, d, r, h)
     | GT -> bal l y d' (add x d r))

  (** val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1) **)

  let rec remove_min l x d r =
    match l with
    | Leaf -> (r, (x, d))
    | Node (ll, lx, ld, lr, _) ->
      let (l', m) = remove_min ll lx ld lr in ((bal l' x d r), m)

  (** val merge : 'a1 tree -> 'a1 tree -> 'a1 tree **)

  let merge s1 s2 =
    match s1 with
    | Leaf -> s2
    | Node (_, _, _, _, _) ->
      (match s2 with
       | Leaf -> s1
       | Node (l2, x2, d2, r2, _) ->
         let (s2', p) = remove_min l2 x2 d2 r2 in
         let (x, d) = p in bal s1 x d s2')

  (** val remove : X.t -> 'a1 tree -> 'a1 tree **)

  let rec remove x = function
  | Leaf -> Leaf
  | Node (l, y, d, r, _) ->
    (match X.compare x y with
     | LT -> bal (remove x l) y d r
     | EQ -> merge l r
     | GT -> bal l y d (remove x r))

  (** val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let rec join l = match l with
  | Leaf -> add
  | Node (ll, lx, ld, lr, lh) ->
    (fun x d ->
      let rec join_aux r = match r with
      | Leaf -> add x d l
      | Node (rl, rx, rd, rr, rh) ->
        if I.gt_le_dec lh (I.add rh I._2)
        then bal ll lx ld (join lr x d r)
        else if I.gt_le_dec rh (I.add lh I._2)
             then bal (join_aux rl) rx rd rr
             else create l x d r
      in join_aux)

  type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                       t_right : 'elt tree }

  (** val t_left : 'a1 triple -> 'a1 tree **)

  let t_left t0 =
    t0.t_left

  (** val t_opt : 'a1 triple -> 'a1 option **)

  let t_opt t0 =
    t0.t_opt

  (** val t_right : 'a1 triple -> 'a1 tree **)

  let t_right t0 =
    t0.t_right

  (** val split : X.t -> 'a1 tree -> 'a1 triple **)

  let rec split x = function
  | Leaf -> { t_left = Leaf; t_opt = None; t_right = Leaf }
  | Node (l, y, d, r, _) ->
    (match X.compare x y with
     | LT ->
       let { t_left = ll; t_opt = o; t_right = rl } = split x l in
       { t_left = ll; t_opt = o; t_right = (join rl y d r) }
     | EQ -> { t_left = l; t_opt = (Some d); t_right = r }
     | GT ->
       let { t_left = rl; t_opt = o; t_right = rr } = split x r in
       { t_left = (join l y d rl); t_opt = o; t_right = rr })

  (** val concat : 'a1 tree -> 'a1 tree -> 'a1 tree **)

  let concat m1 m2 =
    match m1 with
    | Leaf -> m2
    | Node (_, _, _, _, _) ->
      (match m2 with
       | Leaf -> m1
       | Node (l2, x2, d2, r2, _) ->
         let (m2', xd) = remove_min l2 x2 d2 r2 in
         join m1 (fst xd) (snd xd) m2')

  (** val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list **)

  let rec elements_aux acc = function
  | Leaf -> acc
  | Node (l, x, d, r, _) -> elements_aux ((x, d) :: (elements_aux acc r)) l

  (** val elements : 'a1 tree -> (key * 'a1) list **)

  let elements m =
    elements_aux [] m

  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)

  let rec fold f m a =
    match m with
    | Leaf -> a
    | Node (l, x, d, r, _) -> fold f r (f x d (fold f l a))

  type 'elt enumeration =
  | End
  | More of key * 'elt * 'elt tree * 'elt enumeration

  (** val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2 **)

  let rec enumeration_rect f f0 = function
  | End -> f
  | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rect f f0 e1)

  (** val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2 **)

  let rec enumeration_rec f f0 = function
  | End -> f
  | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rec f f0 e1)

  (** val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration **)

  let rec cons m e =
    match m with
    | Leaf -> e
    | Node (l, x, d, r, _) -> cons l (More (x, d, r, e))

  (** val equal_more :
      ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool **)

  let equal_more cmp x1 d1 cont = function
  | End -> false
  | More (x2, d2, r2, e3) ->
    (match X.compare x1 x2 with
     | EQ -> if cmp d1 d2 then cont (cons r2 e3) else false
     | _ -> false)

  (** val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool **)

  let rec equal_cont cmp m1 cont e2 =
    match m1 with
    | Leaf -> cont e2
    | Node (l1, x1, d1, r1, _) ->
      equal_cont cmp l1 (equal_more cmp x1 d1 (equal_cont cmp r1 cont)) e2

  (** val equal_end : 'a1 enumeration -> bool **)

  let equal_end = function
  | End -> true
  | More (_, _, _, _) -> false

  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool **)

  let equal cmp m1 m2 =
    equal_cont cmp m1 equal_end (cons m2 End)

  (** val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)

  let rec map f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, h) -> Node ((map f l), x, (f d), (map f r), h)

  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)

  let rec mapi f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, h) -> Node ((mapi f l), x, (f x d), (mapi f r), h)

  (** val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree **)

  let rec map_option f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, _) ->
    (match f x d with
     | Some d' -> join (map_option f l) x d' (map_option f r)
     | None -> concat (map_option f l) (map_option f r))

  (** val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree **)

  let rec map2_opt f mapl mapr m1 m2 =
    match m1 with
    | Leaf -> mapr m2
    | Node (l1, x1, d1, r1, _) ->
      (match m2 with
       | Leaf -> mapl m1
       | Node (_, _, _, _, _) ->
         let { t_left = l2'; t_opt = o2; t_right = r2' } = split x1 m2 in
         (match f x1 d1 o2 with
          | Some e ->
            join (map2_opt f mapl mapr l1 l2') x1 e
              (map2_opt f mapl mapr r1 r2')
          | None ->
            concat (map2_opt f mapl mapr l1 l2') (map2_opt f mapl mapr r1 r2')))

  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree **)

  let map2 f =
    map2_opt (fun _ d o -> f (Some d) o)
      (map_option (fun _ d -> f (Some d) None))
      (map_option (fun _ d' -> f None (Some d')))

  module Proofs =
   struct
    module MX = OrderedTypeFacts(X)

    module PX = KeyOrderedType(X)

    module L = Raw(X)

    type 'elt coq_R_mem =
    | R_mem_0 of 'elt tree
    | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem
    | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem

    (** val coq_R_mem_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2 **)

    let rec coq_R_mem_rect x f f0 f1 f2 _ _ = function
    | R_mem_0 m -> f m __
    | R_mem_1 (m, l, y, _x, r0, _x0, _res, r1) ->
      f0 m l y _x r0 _x0 __ __ __ _res r1
        (coq_R_mem_rect x f f0 f1 f2 l _res r1)
    | R_mem_2 (m, l, y, _x, r0, _x0) -> f1 m l y _x r0 _x0 __ __ __
    | R_mem_3 (m, l, y, _x, r0, _x0, _res, r1) ->
      f2 m l y _x r0 _x0 __ __ __ _res r1
        (coq_R_mem_rect x f f0 f1 f2 r0 _res r1)

    (** val coq_R_mem_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2 **)

    let rec coq_R_mem_rec x f f0 f1 f2 _ _ = function
    | R_mem_0 m -> f m __
    | R_mem_1 (m, l, y, _x, r0, _x0, _res, r1) ->
      f0 m l y _x r0 _x0 __ __ __ _res r1
        (coq_R_mem_rec x f f0 f1 f2 l _res r1)
    | R_mem_2 (m, l, y, _x, r0, _x0) -> f1 m l y _x r0 _x0 __ __ __
    | R_mem_3 (m, l, y, _x, r0, _x0, _res, r1) ->
      f2 m l y _x r0 _x0 __ __ __ _res r1
        (coq_R_mem_rec x f f0 f1 f2 r0 _res r1)

    type 'elt coq_R_find =
    | R_find_0 of 'elt tree
    | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find
    | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find

    (** val coq_R_find_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)

    let rec coq_R_find_rect x f f0 f1 f2 _ _ = function
    | R_find_0 m -> f m __
    | R_find_1 (m, l, y, d, r0, _x, _res, r1) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_find_rect x f f0 f1 f2 l _res r1)
    | R_find_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_find_3 (m, l, y, d, r0, _x, _res, r1) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_find_rect x f f0 f1 f2 r0 _res r1)

    (** val coq_R_find_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)

    let rec coq_R_find_rec x f f0 f1 f2 _ _ = function
    | R_find_0 m -> f m __
    | R_find_1 (m, l, y, d, r0, _x, _res, r1) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_find_rec x f f0 f1 f2 l _res r1)
    | R_find_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_find_3 (m, l, y, d, r0, _x, _res, r1) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_find_rec x f f0 f1 f2 r0 _res r1)

    type 'elt coq_R_bal =
    | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
    | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
       I.t
    | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
    | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
       I.t
    | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

    (** val coq_R_bal_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
        'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)

    let coq_R_bal_rect f f0 f1 f2 f3 f4 f5 f6 f7 _ _ _ _ _ = function
    | R_bal_0 (l, x, d, r) -> f l x d r __ __ __
    | R_bal_1 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f0 l x d r __ __ x0 x1 x2 x3 x4 __ __ __
    | R_bal_2 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f1 l x d r __ __ x0 x1 x2 x3 x4 __ __ __ __
    | R_bal_3 (l, x, d, r, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
      f2 l x d r __ __ x0 x1 x2 x3 x4 __ __ __ x5 x6 x7 x8 x9 __
    | R_bal_4 (l, x, d, r) -> f3 l x d r __ __ __ __ __
    | R_bal_5 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f4 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __
    | R_bal_6 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f5 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __ __
    | R_bal_7 (l, x, d, r, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
      f6 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __ x5 x6 x7 x8 x9 __
    | R_bal_8 (l, x, d, r) -> f7 l x d r __ __ __ __

    (** val coq_R_bal_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
        'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)

    let coq_R_bal_rec f f0 f1 f2 f3 f4 f5 f6 f7 _ _ _ _ _ = function
    | R_bal_0 (l, x, d, r) -> f l x d r __ __ __
    | R_bal_1 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f0 l x d r __ __ x0 x1 x2 x3 x4 __ __ __
    | R_bal_2 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f1 l x d r __ __ x0 x1 x2 x3 x4 __ __ __ __
    | R_bal_3 (l, x, d, r, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
      f2 l x d r __ __ x0 x1 x2 x3 x4 __ __ __ x5 x6 x7 x8 x9 __
    | R_bal_4 (l, x, d, r) -> f3 l x d r __ __ __ __ __
    | R_bal_5 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f4 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __
    | R_bal_6 (l, x, d, r, x0, x1, x2, x3, x4) ->
      f5 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __ __
    | R_bal_7 (l, x, d, r, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
      f6 l x d r __ __ __ __ x0 x1 x2 x3 x4 __ __ __ x5 x6 x7 x8 x9 __
    | R_bal_8 (l, x, d, r) -> f7 l x d r __ __ __ __

    type 'elt coq_R_add =
    | R_add_0 of 'elt tree
    | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add
    | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add

    (** val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2 **)

    let rec coq_R_add_rect x d f f0 f1 f2 _ _ = function
    | R_add_0 m -> f m __
    | R_add_1 (m, l, y, d', r0, h, _res, r1) ->
      f0 m l y d' r0 h __ __ __ _res r1
        (coq_R_add_rect x d f f0 f1 f2 l _res r1)
    | R_add_2 (m, l, y, d', r0, h) -> f1 m l y d' r0 h __ __ __
    | R_add_3 (m, l, y, d', r0, h, _res, r1) ->
      f2 m l y d' r0 h __ __ __ _res r1
        (coq_R_add_rect x d f f0 f1 f2 r0 _res r1)

    (** val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2 **)

    let rec coq_R_add_rec x d f f0 f1 f2 _ _ = function
    | R_add_0 m -> f m __
    | R_add_1 (m, l, y, d', r0, h, _res, r1) ->
      f0 m l y d' r0 h __ __ __ _res r1
        (coq_R_add_rec x d f f0 f1 f2 l _res r1)
    | R_add_2 (m, l, y, d', r0, h) -> f1 m l y d' r0 h __ __ __
    | R_add_3 (m, l, y, d', r0, h, _res, r1) ->
      f2 m l y d' r0 h __ __ __ _res r1
        (coq_R_add_rec x d f f0 f1 f2 r0 _res r1)

    type 'elt coq_R_remove_min =
    | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
    | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
       key * 'elt * 'elt tree * I.t * ('elt tree * (key * 'elt))
       * 'elt coq_R_remove_min * 'elt tree * (key * 'elt)

    (** val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1
        tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 **)

    let rec coq_R_remove_min_rect f f0 _ _ _ _ _ = function
    | R_remove_min_0 (l, x, d, r) -> f l x d r __
    | R_remove_min_1 (l, x, d, r, ll, lx, ld, lr, _x, _res, r1, l', m) ->
      f0 l x d r ll lx ld lr _x __ _res r1
        (coq_R_remove_min_rect f f0 ll lx ld lr _res r1) l' m __

    (** val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1
        tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 **)

    let rec coq_R_remove_min_rec f f0 _ _ _ _ _ = function
    | R_remove_min_0 (l, x, d, r) -> f l x d r __
    | R_remove_min_1 (l, x, d, r, ll, lx, ld, lr, _x, _res, r1, l', m) ->
      f0 l x d r ll lx ld lr _x __ _res r1
        (coq_R_remove_min_rec f f0 ll lx ld lr _res r1) l' m __

    type 'elt coq_R_merge =
    | R_merge_0 of 'elt tree * 'elt tree
    | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t
    | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t * 'elt tree * key * 'elt * 'elt tree * I.t * 'elt tree
       * (key * 'elt) * key * 'elt

    (** val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_merge -> 'a2 **)

    let coq_R_merge_rect f f0 f1 _ _ _ = function
    | R_merge_0 (s1, s2) -> f s1 s2 __
    | R_merge_1 (s1, s2, _x, _x0, _x1, _x2, _x3) ->
      f0 s1 s2 _x _x0 _x1 _x2 _x3 __ __
    | R_merge_2 (s1, s2, _x, _x0, _x1, _x2, _x3, l2, x2, d2, r2, _x4, s2', p,
                 x, d) ->
      f1 s1 s2 _x _x0 _x1 _x2 _x3 __ l2 x2 d2 r2 _x4 __ s2' p __ x d __

    (** val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_merge -> 'a2 **)

    let coq_R_merge_rec f f0 f1 _ _ _ = function
    | R_merge_0 (s1, s2) -> f s1 s2 __
    | R_merge_1 (s1, s2, _x, _x0, _x1, _x2, _x3) ->
      f0 s1 s2 _x _x0 _x1 _x2 _x3 __ __
    | R_merge_2 (s1, s2, _x, _x0, _x1, _x2, _x3, l2, x2, d2, r2, _x4, s2', p,
                 x, d) ->
      f1 s1 s2 _x _x0 _x1 _x2 _x3 __ l2 x2 d2 r2 _x4 __ s2' p __ x d __

    type 'elt coq_R_remove =
    | R_remove_0 of 'elt tree
    | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove
    | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove

    (** val coq_R_remove_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)

    let rec coq_R_remove_rect x f f0 f1 f2 _ _ = function
    | R_remove_0 m -> f m __
    | R_remove_1 (m, l, y, d, r0, _x, _res, r1) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_remove_rect x f f0 f1 f2 l _res r1)
    | R_remove_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_remove_3 (m, l, y, d, r0, _x, _res, r1) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_remove_rect x f f0 f1 f2 r0 _res r1)

    (** val coq_R_remove_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)

    let rec coq_R_remove_rec x f f0 f1 f2 _ _ = function
    | R_remove_0 m -> f m __
    | R_remove_1 (m, l, y, d, r0, _x, _res, r1) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_remove_rec x f f0 f1 f2 l _res r1)
    | R_remove_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_remove_3 (m, l, y, d, r0, _x, _res, r1) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_remove_rec x f f0 f1 f2 r0 _res r1)

    type 'elt coq_R_concat =
    | R_concat_0 of 'elt tree * 'elt tree
    | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t
    | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * (key * 'elt)

    (** val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_concat -> 'a2 **)

    let coq_R_concat_rect f f0 f1 _ _ _ = function
    | R_concat_0 (m1, m2) -> f m1 m2 __
    | R_concat_1 (m1, m2, _x, _x0, _x1, _x2, _x3) ->
      f0 m1 m2 _x _x0 _x1 _x2 _x3 __ __
    | R_concat_2 (m1, m2, _x, _x0, _x1, _x2, _x3, l2, x2, d2, r2, _x4, m2', xd) ->
      f1 m1 m2 _x _x0 _x1 _x2 _x3 __ l2 x2 d2 r2 _x4 __ m2' xd __

    (** val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_concat -> 'a2 **)

    let coq_R_concat_rec f f0 f1 _ _ _ = function
    | R_concat_0 (m1, m2) -> f m1 m2 __
    | R_concat_1 (m1, m2, _x, _x0, _x1, _x2, _x3) ->
      f0 m1 m2 _x _x0 _x1 _x2 _x3 __ __
    | R_concat_2 (m1, m2, _x, _x0, _x1, _x2, _x3, l2, x2, d2, r2, _x4, m2', xd) ->
      f1 m1 m2 _x _x0 _x1 _x2 _x3 __ l2 x2 d2 r2 _x4 __ m2' xd __

    type 'elt coq_R_split =
    | R_split_0 of 'elt tree
    | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree
    | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree

    (** val coq_R_split_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 **)

    let rec coq_R_split_rect x f f0 f1 f2 _ _ = function
    | R_split_0 m -> f m __
    | R_split_1 (m, l, y, d, r0, _x, _res, r1, ll, o, rl) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_split_rect x f f0 f1 f2 l _res r1) ll o rl __
    | R_split_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_split_3 (m, l, y, d, r0, _x, _res, r1, rl, o, rr) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_split_rect x f f0 f1 f2 r0 _res r1) rl o rr __

    (** val coq_R_split_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 **)

    let rec coq_R_split_rec x f f0 f1 f2 _ _ = function
    | R_split_0 m -> f m __
    | R_split_1 (m, l, y, d, r0, _x, _res, r1, ll, o, rl) ->
      f0 m l y d r0 _x __ __ __ _res r1
        (coq_R_split_rec x f f0 f1 f2 l _res r1) ll o rl __
    | R_split_2 (m, l, y, d, r0, _x) -> f1 m l y d r0 _x __ __ __
    | R_split_3 (m, l, y, d, r0, _x, _res, r1, rl, o, rr) ->
      f2 m l y d r0 _x __ __ __ _res r1
        (coq_R_split_rec x f f0 f1 f2 r0 _res r1) rl o rr __

    type ('elt, 'x) coq_R_map_option =
    | R_map_option_0 of 'elt tree
    | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
       * ('elt, 'x) coq_R_map_option
    | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
       * ('elt, 'x) coq_R_map_option

    (** val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 **)

    let rec coq_R_map_option_rect f f0 f1 f2 _ _ = function
    | R_map_option_0 m -> f0 m __
    | R_map_option_1 (m, l, x, d, r0, _x, d', _res0, r1, _res, r2) ->
      f1 m l x d r0 _x __ d' __ _res0 r1
        (coq_R_map_option_rect f f0 f1 f2 l _res0 r1) _res r2
        (coq_R_map_option_rect f f0 f1 f2 r0 _res r2)
    | R_map_option_2 (m, l, x, d, r0, _x, _res0, r1, _res, r2) ->
      f2 m l x d r0 _x __ __ _res0 r1
        (coq_R_map_option_rect f f0 f1 f2 l _res0 r1) _res r2
        (coq_R_map_option_rect f f0 f1 f2 r0 _res r2)

    (** val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 **)

    let rec coq_R_map_option_rec f f0 f1 f2 _ _ = function
    | R_map_option_0 m -> f0 m __
    | R_map_option_1 (m, l, x, d, r0, _x, d', _res0, r1, _res, r2) ->
      f1 m l x d r0 _x __ d' __ _res0 r1
        (coq_R_map_option_rec f f0 f1 f2 l _res0 r1) _res r2
        (coq_R_map_option_rec f f0 f1 f2 r0 _res r2)
    | R_map_option_2 (m, l, x, d, r0, _x, _res0, r1, _res, r2) ->
      f2 m l x d r0 _x __ __ _res0 r1
        (coq_R_map_option_rec f f0 f1 f2 l _res0 r1) _res r2
        (coq_R_map_option_rec f f0 f1 f2 r0 _res r2)

    type ('elt, 'x0, 'x) coq_R_map2_opt =
    | R_map2_opt_0 of 'elt tree * 'x0 tree
    | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
       * 'elt tree * I.t
    | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'x0 tree * key * 'x0 * 'x0 tree * I.t * 'x0 tree
       * 'x0 option * 'x0 tree * 'x * 'x tree
       * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
       * ('elt, 'x0, 'x) coq_R_map2_opt
    | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'x0 tree * key * 'x0 * 'x0 tree * I.t * 'x0 tree
       * 'x0 option * 'x0 tree * 'x tree * ('elt, 'x0, 'x) coq_R_map2_opt
       * 'x tree * ('elt, 'x0, 'x) coq_R_map2_opt

    (** val coq_R_map2_opt_rect :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t ->
        __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key ->
        'a2 -> 'a2 tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)

    let rec coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 _ _ _ = function
    | R_map2_opt_0 (m1, m2) -> f0 m1 m2 __
    | R_map2_opt_1 (m1, m2, l1, x1, d1, r1, _x) ->
      f1 m1 m2 l1 x1 d1 r1 _x __ __
    | R_map2_opt_2 (m1, m2, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', e, _res0, r0, _res, r2) ->
      f2 m1 m2 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e __
        _res0 r0
        (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' _res0 r0) _res r2
        (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' _res r2)
    | R_map2_opt_3 (m1, m2, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', _res0, r0, _res, r2) ->
      f3 m1 m2 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __
        _res0 r0
        (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' _res0 r0) _res r2
        (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' _res r2)

    (** val coq_R_map2_opt_rec :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t ->
        __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key ->
        'a2 -> 'a2 tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)

    let rec coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 _ _ _ = function
    | R_map2_opt_0 (m1, m2) -> f0 m1 m2 __
    | R_map2_opt_1 (m1, m2, l1, x1, d1, r1, _x) ->
      f1 m1 m2 l1 x1 d1 r1 _x __ __
    | R_map2_opt_2 (m1, m2, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', e, _res0, r0, _res, r2) ->
      f2 m1 m2 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e __
        _res0 r0 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' _res0 r0)
        _res r2 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' _res r2)
    | R_map2_opt_3 (m1, m2, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', _res0, r0, _res, r2) ->
      f3 m1 m2 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __
        _res0 r0 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' _res0 r0)
        _res r2 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' _res r2)

    (** val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)

    let fold' f s =
      L.fold f (elements s)

    (** val flatten_e : 'a1 enumeration -> (key * 'a1) list **)

    let rec flatten_e = function
    | End -> []
    | More (x, e0, t0, r) -> (x, e0) :: (app (elements t0) (flatten_e r))
   end
 end

module IntMake =
 functor (I:Int) ->
 functor (X:OrderedType) ->
 struct
  module E = X

  module Raw = Coq_Raw(I)(X)

  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)

  (** val this : 'a1 bst -> 'a1 Raw.tree **)

  let this b =
    b

  type 'elt t = 'elt bst

  type key = E.t

  (** val empty : 'a1 t **)

  let empty =
    Raw.empty

  (** val is_empty : 'a1 t -> bool **)

  let is_empty =
    Raw.is_empty

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let add =
    Raw.add

  (** val remove : key -> 'a1 t -> 'a1 t **)

  let remove =
    Raw.remove

  (** val mem : key -> 'a1 t -> bool **)

  let mem =
    Raw.mem

  (** val find : key -> 'a1 t -> 'a1 option **)

  let find =
    Raw.find

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    Raw.map

  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let mapi =
    Raw.mapi

  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let map2 =
    Raw.map2

  (** val elements : 'a1 t -> (key * 'a1) list **)

  let elements =
    Raw.elements

  (** val cardinal : 'a1 t -> int **)

  let cardinal =
    Raw.cardinal

  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold =
    Raw.fold

  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let equal =
    Raw.equal
 end

module Make =
 functor (X:OrderedType) ->
 IntMake(Z_as_Int)(X)

module Map = Make(StringOT)

type state = { maxSuccessTests : int; maxDiscardedTests : int;
               maxShrinkNo : int; computeSize : (int -> int -> int);
               numSuccessTests : int; numDiscardedTests : int;
               labels : int Map.t; expectedFailure : bool;
               randomSeed0 : randomSeed; numSuccessShrinks : int;
               numTryShrinks : int; stDoAnalysis : bool }

(** val updTryShrinks : state -> (int -> int) -> state **)

let updTryShrinks st f =
  let { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo = ms;
    computeSize = cs; numSuccessTests = nst; numDiscardedTests = ndt;
    labels = ls; expectedFailure = e; randomSeed0 = r; numSuccessShrinks =
    nss; numTryShrinks = nts; stDoAnalysis = ana } = st
  in
  { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo = ms;
  computeSize = cs; numSuccessTests = nst; numDiscardedTests = ndt; labels =
  ls; expectedFailure = e; randomSeed0 = r; numSuccessShrinks = nss;
  numTryShrinks = (f nts); stDoAnalysis = ana }

(** val updSuccessShrinks : state -> (int -> int) -> state **)

let updSuccessShrinks st f =
  let { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo = ms;
    computeSize = cs; numSuccessTests = nst; numDiscardedTests = ndt;
    labels = ls; expectedFailure = e; randomSeed0 = r; numSuccessShrinks =
    nss; numTryShrinks = nts; stDoAnalysis = ana } = st
  in
  { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo = ms;
  computeSize = cs; numSuccessTests = nst; numDiscardedTests = ndt; labels =
  ls; expectedFailure = e; randomSeed0 = r; numSuccessShrinks = (f nss);
  numTryShrinks = nts; stDoAnalysis = ana }

type 'a genSized =
  int -> 'a g
  (* singleton inductive, whose constructor was Build_GenSized *)

type 'a gen = 'a g
  (* singleton inductive, whose constructor was Build_Gen *)

(** val genOfGenSized : 'a1 genSized -> 'a1 gen **)

let genOfGenSized h =
  sized (Obj.magic producerGen) h

type callbackKind =
| Counterexample
| NotCounterexample

type smallResult =
| MkSmallResult of bool option * bool * char list * bool * char list list
   * char list option

type callback =
| PostTest of callbackKind * (state -> smallResult -> int)
| PostFinalFailure of callbackKind * (state -> smallResult -> int)

type result = { ok : bool option; expect : bool; reason : char list;
                interrupted : bool; stamp : char list list;
                callbacks : callback list; result_tag : char list option }

(** val succeeded : result **)

let succeeded =
  { ok = (Some true); expect = true; reason = []; interrupted = false;
    stamp = []; callbacks = []; result_tag = None }

(** val failed : result **)

let failed =
  { ok = (Some false); expect = true; reason = []; interrupted = false;
    stamp = []; callbacks = []; result_tag = None }

(** val rejected : result **)

let rejected =
  { ok = None; expect = true; reason = []; interrupted = false; stamp = [];
    callbacks = []; result_tag = None }

(** val updReason : result -> char list -> result **)

let updReason r s' =
  let { ok = o; expect = e; reason = _; interrupted = i; stamp = s;
    callbacks = c; result_tag = t0 } = r
  in
  { ok = o; expect = e; reason = s'; interrupted = i; stamp = s; callbacks =
  c; result_tag = t0 }

(** val addCallback : result -> callback -> result **)

let addCallback res c =
  let { ok = o; expect = e; reason = r; interrupted = i; stamp = s;
    callbacks = cs; result_tag = t0 } = res
  in
  { ok = o; expect = e; reason = r; interrupted = i; stamp = s; callbacks =
  (c :: cs); result_tag = t0 }

type qProp =
  result rose
  (* singleton inductive, whose constructor was MkProp *)

type checker = qProp g

type 'a checkable =
  'a -> checker
  (* singleton inductive, whose constructor was Build_Checkable *)

(** val liftBool : bool -> result **)

let liftBool = function
| true -> succeeded
| false ->
  updReason failed
    ('F'::('a'::('l'::('s'::('i'::('f'::('i'::('a'::('b'::('l'::('e'::[])))))))))))

(** val mapProp : 'a1 checkable -> (qProp -> qProp) -> 'a1 -> checker **)

let mapProp x f prop =
  fmap (functor_Monad (Obj.magic monadGen)) f (x prop)

(** val mapRoseResult :
    'a1 checkable -> (result rose -> result rose) -> 'a1 -> checker **)

let mapRoseResult =
  mapProp

(** val mapTotalResult :
    'a1 checkable -> (result -> result) -> 'a1 -> checker **)

let mapTotalResult x f =
  mapRoseResult x (fmapRose f)

(** val testResult : result checkable **)

let testResult r =
  ret (Obj.magic monadGen) (returnRose r)

(** val testBool : bool checkable **)

let testBool b =
  testResult (liftBool b)

(** val testUnit : unit checkable **)

let testUnit _ =
  testResult rejected

(** val testChecker : checker checkable **)

let testChecker x =
  x

(** val callback0 : 'a1 checkable -> callback -> 'a1 -> checker **)

let callback0 h cb =
  mapTotalResult h (fun r -> addCallback r cb)

(** val printTestCase : 'a1 checkable -> char list -> 'a1 -> checker **)

let printTestCase h s p =
  callback0 h (PostFinalFailure (Counterexample, (fun _ _ -> trace s 0))) p

(** val forAll :
    'a2 checkable -> 'a1 show -> 'a1 g -> ('a1 -> 'a2) -> checker **)

let forAll x h gen0 pf =
  bindGen gen0 (fun x0 -> printTestCase x (append (h x0) newline) (pf x0))

(** val gte : int -> int -> bool **)

let gte = (>=)

type args = { replay : (randomSeed * int) option; maxSuccess : int;
              maxDiscard : int; maxShrinks : int; maxSize : int;
              chatty : bool; analysis : bool }

(** val updMaxSuccess : args -> int -> args **)

let updMaxSuccess a x =
  let { replay = r; maxSuccess = _; maxDiscard = md; maxShrinks = msh;
    maxSize = msz; chatty = c; analysis = an } = a
  in
  { replay = r; maxSuccess = x; maxDiscard = md; maxShrinks = msh; maxSize =
  msz; chatty = c; analysis = an }

(** val updMaxDiscard : args -> int -> args **)

let updMaxDiscard a x =
  let { replay = r; maxSuccess = msc; maxDiscard = _; maxShrinks = msh;
    maxSize = msz; chatty = c; analysis = an } = a
  in
  { replay = r; maxSuccess = msc; maxDiscard = x; maxShrinks = msh; maxSize =
  msz; chatty = c; analysis = an }

(** val updAnalysis : args -> bool -> args **)

let updAnalysis a b =
  let { replay = r; maxSuccess = msc; maxDiscard = md; maxShrinks = msh;
    maxSize = msz; chatty = c; analysis = _ } = a
  in
  { replay = r; maxSuccess = msc; maxDiscard = md; maxShrinks = msh;
  maxSize = msz; chatty = c; analysis = b }

type result0 =
| Success of int * int * (char list * int) list * char list
| GaveUp of int * (char list * int) list * char list
| Failure of int * int * int * randomSeed * int * char list
   * (char list * int) list * char list
| NoExpectedFailure of int * (char list * int) list * char list

(** val defNumTests : int **)

let defNumTests = 10000

(** val defNumDiscards : int **)

let defNumDiscards = (2 * defNumTests)

(** val defNumShrinks : int **)

let defNumShrinks = 1000

(** val defSize : int **)

let defSize = 7

(** val doAnalysis : bool **)

let doAnalysis =
  false

(** val stdArgs : args **)

let stdArgs =
  { replay = None; maxSuccess = defNumTests; maxDiscard = defNumDiscards;
    maxShrinks = defNumShrinks; maxSize = defSize; chatty = true; analysis =
    doAnalysis }

(** val roundTo : int -> int -> int **)

let roundTo n0 m =
  mul (Nat.div n0 m) m

(** val computeSize'' : int -> int -> int -> int -> int **)

let computeSize'' maxSize_ maxSuccess_ n0 d =
  if (||)
       ((||) (Nat.leb (add (roundTo n0 maxSize_) maxSize_) maxSuccess_)
         (Nat.leb maxSuccess_ n0))
       (Nat.eqb (Nat.modulo maxSuccess_ maxSize_) 0)
  then Stdlib.min
         (add (Nat.modulo n0 maxSize_)
           (Nat.div d (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ 0)))))))))))) maxSize_
  else Stdlib.min
         (Nat.div (mul (Nat.modulo n0 maxSize_) maxSize_)
           (add (Nat.modulo maxSuccess_ maxSize_)
             (Nat.div d (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ 0))))))))))))) maxSize_

(** val computeSize' : args -> int -> int -> int **)

let computeSize' a n0 d =
  computeSize'' a.maxSize a.maxSuccess n0 d

(** val at0 : (int -> int -> int) -> int -> int -> int -> int **)

let at0 f s n0 d =
  if (&&) (Nat.eqb n0 0) (Nat.eqb d 0) then s else f n0 d

(** val insertBy : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec insertBy compare1 x l = match l with
| [] -> x :: []
| h :: t0 -> if compare1 x h then x :: l else h :: (insertBy compare1 x t0)

(** val insSortBy : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list **)

let rec insSortBy compare1 = function
| [] -> []
| h :: t0 -> insertBy compare1 h (insSortBy compare1 t0)

(** val summary : state -> (char list * int) list **)

let summary st =
  let res = Map.fold (fun key0 elem acc -> (key0, elem) :: acc) st.labels []
  in
  insSortBy (fun x y -> Nat.leb (snd y) (snd x)) res

(** val doneTesting : state -> result0 **)

let doneTesting st =
  if st.expectedFailure
  then Success ((add st.numSuccessTests (Stdlib.Int.succ 0)),
         st.numDiscardedTests, (summary st),
         (if st.stDoAnalysis
          then append
                 ('"'::('r'::('e'::('s'::('u'::('l'::('t'::('"'::(':'::(' '::('"'::('F'::('i'::('n'::('i'::('s'::('h'::('e'::('d'::('"'::(','::(' '::('"'::('t'::('e'::('s'::('t'::('s'::('"'::(':'::(' '::[])))))))))))))))))))))))))))))))
                 (append (showNat st.numSuccessTests)
                   (append
                     (','::(' '::('"'::('d'::('i'::('s'::('c'::('a'::('r'::('d'::('s'::('"'::(':'::(' '::[]))))))))))))))
                     (showNat st.numDiscardedTests)))
          else append
                 ('+'::('+'::('+'::(' '::('P'::('a'::('s'::('s'::('e'::('d'::(' '::[])))))))))))
                 (append (showNat st.numSuccessTests)
                   (append
                     (' '::('t'::('e'::('s'::('t'::('s'::(' '::('('::[]))))))))
                     (append (showNat st.numDiscardedTests)
                       (append
                         (' '::('d'::('i'::('s'::('c'::('a'::('r'::('d'::('s'::(')'::[]))))))))))
                         newline))))))
  else NoExpectedFailure (st.numSuccessTests, (summary st),
         (if st.stDoAnalysis
          then append
                 ('"'::('r'::('e'::('s'::('u'::('l'::('t'::('"'::(':'::(' '::('"'::('E'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::('F'::('a'::('i'::('l'::('u'::('r'::('e'::('"'::(','::(' '::('"'::('t'::('e'::('s'::('t'::('s'::('"'::(':'::(' '::[]))))))))))))))))))))))))))))))))))))))
                 (showNat st.numSuccessTests)
          else append
                 ('*'::('*'::('*'::(' '::('F'::('a'::('i'::('l'::('e'::('d'::('!'::(' '::('P'::('a'::('s'::('s'::('e'::('d'::(' '::[])))))))))))))))))))
                 (append (showNat st.numSuccessTests)
                   (append
                     (' '::('t'::('e'::('s'::('t'::('s'::(' '::('('::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('F'::('a'::('i'::('l'::('u'::('r'::('e'::(')'::[])))))))))))))))))))))))))
                     newline))))

(** val giveUp : state -> result0 **)

let giveUp st =
  GaveUp (st.numSuccessTests, (summary st),
    (if st.stDoAnalysis
     then append
            ('"'::('r'::('e'::('s'::('u'::('l'::('t'::('"'::(':'::(' '::('"'::('G'::('a'::('v'::('e'::('U'::('p'::('"'::(','::(' '::('"'::('t'::('e'::('s'::('t'::('s'::('"'::(':'::[]))))))))))))))))))))))))))))
            (append (showNat st.numSuccessTests)
              (append
                (','::(' '::('"'::('d'::('i'::('s'::('c'::('a'::('r'::('d'::('s'::('"'::(':'::(' '::[]))))))))))))))
                (showNat st.numDiscardedTests)))
     else append
            ('*'::('*'::('*'::(' '::('G'::('a'::('v'::('e'::(' '::('u'::('p'::('!'::(' '::('P'::('a'::('s'::('s'::('e'::('d'::(' '::('o'::('n'::('l'::('y'::(' '::[])))))))))))))))))))))))))
            (append (showNat st.numSuccessTests)
              (append (' '::('t'::('e'::('s'::('t'::('s'::[]))))))
                (append newline
                  (append
                    ('D'::('i'::('s'::('c'::('a'::('r'::('d'::('e'::('d'::(':'::(' '::[])))))))))))
                    (append (showNat st.numDiscardedTests) newline)))))))

(** val callbackPostTest : state -> result -> int **)

let callbackPostTest st res =
  let { ok = o; expect = e; reason = r; interrupted = i; stamp = s;
    callbacks = c; result_tag = t0 } = res
  in
  fold_left (fun acc callback1 ->
    match callback1 with
    | PostTest (_, call) ->
      add (call st (MkSmallResult (o, e, r, i, s, t0))) acc
    | PostFinalFailure (_, _) -> acc) c 0

(** val callbackPostFinalFailure : state -> result -> int **)

let callbackPostFinalFailure st res =
  let { ok = o; expect = e; reason = r; interrupted = i; stamp = s;
    callbacks = c; result_tag = t0 } = res
  in
  fold_left (fun acc callback1 ->
    match callback1 with
    | PostTest (_, _) -> acc
    | PostFinalFailure (_, call) ->
      add (call st (MkSmallResult (o, e, r, i, s, t0))) acc) c 0

(** val localMin : state -> result rose -> int * result **)

let rec localMin st = function
| MkRose (res, ts) ->
  let rec localMin' st0 = function
  | [] ->
    let zero = callbackPostFinalFailure st0 res in
    ((add st0.numSuccessShrinks zero), res)
  | r' :: ts' ->
    let MkRose (res', _) = r' in
    let zero = callbackPostTest st0 res in
    (match res'.ok with
     | Some x ->
       let consistent_tags =
         match res.result_tag with
         | Some t1 ->
           (match res'.result_tag with
            | Some t2 -> if string_dec t1 t2 then true else false
            | None -> false)
         | None -> (match res'.result_tag with
                    | Some _ -> false
                    | None -> true)
       in
       if (&&) (negb x) consistent_tags
       then localMin
              (updSuccessShrinks st0 (fun x0 ->
                add (add x0 (Stdlib.Int.succ 0)) zero)) r'
       else localMin'
              (updTryShrinks st0 (fun x0 -> add x0 (Stdlib.Int.succ 0))) ts'
     | None ->
       localMin' (updTryShrinks st0 (fun x -> add x (Stdlib.Int.succ 0))) ts')
  in localMin' st (force ts)

(** val runATest : state -> (int -> randomSeed -> qProp) -> int -> result0 **)

let rec runATest st f maxSteps =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> giveUp st)
    (fun maxSteps' ->
    let size = st.computeSize st.numSuccessTests st.numDiscardedTests in
    let (rnd1, rnd2) = randomSplit st.randomSeed0 in
    let test0 = fun st0 ->
      if gte st0.numSuccessTests st0.maxSuccessTests
      then doneTesting st0
      else if gte st0.numDiscardedTests st0.maxDiscardedTests
           then giveUp st0
           else runATest st0 f maxSteps'
    in
    let { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo = ms;
      computeSize = cs; numSuccessTests = nst; numDiscardedTests = ndt;
      labels = ls; expectedFailure = _; randomSeed0 = r; numSuccessShrinks =
      nss; numTryShrinks = nts; stDoAnalysis = ana } = st
    in
    let MkRose (res, ts) = f size rnd1 in
    let res_cb = callbackPostTest st res in
    let { ok = ok0; expect = e; reason = reas; interrupted = _; stamp = s;
      callbacks = _; result_tag = t0 } = res
    in
    (match ok0 with
     | Some x ->
       if x
       then let ls' =
              match s with
              | [] -> ls
              | _ :: _ ->
                let s_to_add =
                  ShowFunctions.string_concat
                    (ShowFunctions.intersperse (' '::(','::(' '::[]))) s)
                in
                (match Map.find s_to_add ls with
                 | Some k -> Map.add s_to_add (add k (Stdlib.Int.succ 0)) ls
                 | None ->
                   Map.add s_to_add (add res_cb (Stdlib.Int.succ 0)) ls)
            in
            test0 { maxSuccessTests = mst; maxDiscardedTests = mdt;
              maxShrinkNo = ms; computeSize = cs; numSuccessTests =
              (add nst (Stdlib.Int.succ 0)); numDiscardedTests = ndt;
              labels = ls'; expectedFailure = e; randomSeed0 = rnd2;
              numSuccessShrinks = nss; numTryShrinks = nts; stDoAnalysis =
              ana }
       else let tag_text =
              match t0 with
              | Some s0 ->
                append ('T'::('a'::('g'::(':'::(' '::[]))))) (append s0 nl)
              | None -> []
            in
            let pre =
              if ana
              then if res.expect
                   then '"'::('r'::('e'::('s'::('u'::('l'::('t'::('"'::(':'::(' '::('"'::('f'::('a'::('i'::('l'::('e'::('d'::('"'::(','::(' '::[])))))))))))))))))))
                   else '"'::('r'::('e'::('s'::('u'::('l'::('t'::('"'::(':'::(' '::('"'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::('_'::('f'::('a'::('i'::('l'::('u'::('r'::('e'::('"'::(' '::[]))))))))))))))))))))))))))))
              else if res.expect
                   then '*'::('*'::('*'::(' '::('F'::('a'::('i'::('l'::('e'::('d'::(' '::[]))))))))))
                   else '+'::('+'::('+'::(' '::('F'::('a'::('i'::('l'::('e'::('d'::(' '::('('::('a'::('s'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(')'::(' '::[]))))))))))))))))))))))))
            in
            let (numShrinks, _) = localMin st (MkRose (res, ts)) in
            let suf =
              if ana
              then append
                     ('"'::('t'::('e'::('s'::('t'::('s'::('"'::(':'::(' '::[])))))))))
                     (append (showNat (Stdlib.Int.succ nst))
                       (append
                         (','::(' '::('"'::('s'::('h'::('r'::('i'::('n'::('k'::('s'::('"'::(':'::(' '::[])))))))))))))
                         (append (showNat numShrinks)
                           (append
                             (','::(' '::('"'::('d'::('i'::('s'::('c'::('a'::('r'::('d'::('s'::('"'::(':'::(' '::[]))))))))))))))
                             (showNat ndt)))))
              else append ('a'::('f'::('t'::('e'::('r'::(' '::[]))))))
                     (append (showNat (Stdlib.Int.succ nst))
                       (append
                         (' '::('t'::('e'::('s'::('t'::('s'::(' '::('a'::('n'::('d'::(' '::[])))))))))))
                         (append (showNat numShrinks)
                           (append
                             (' '::('s'::('h'::('r'::('i'::('n'::('k'::('s'::('.'::(' '::('('::[])))))))))))
                             (append (showNat ndt)
                               (' '::('d'::('i'::('s'::('c'::('a'::('r'::('d'::('s'::(')'::[])))))))))))))))
            in
            if negb res.expect
            then Success ((add nst (Stdlib.Int.succ 0)), ndt, (summary st),
                   (append tag_text (append pre suf)))
            else Failure ((add nst (Stdlib.Int.succ 0)), numShrinks, ndt, r,
                   size, (append tag_text (append pre suf)), (summary st),
                   reas)
     | None ->
       let ls' =
         match s with
         | [] -> ls
         | _ :: _ ->
           let s_to_add =
             append
               ('('::('D'::('i'::('s'::('c'::('a'::('r'::('d'::('e'::('d'::(')'::(' '::[]))))))))))))
               (ShowFunctions.string_concat
                 (ShowFunctions.intersperse (' '::(','::(' '::[]))) s))
           in
           (match Map.find s_to_add ls with
            | Some k -> Map.add s_to_add (add k (Stdlib.Int.succ 0)) ls
            | None -> Map.add s_to_add (add res_cb (Stdlib.Int.succ 0)) ls)
       in
       test0 { maxSuccessTests = mst; maxDiscardedTests = mdt; maxShrinkNo =
         ms; computeSize = cs; numSuccessTests = nst; numDiscardedTests =
         (Stdlib.Int.succ ndt); labels = ls'; expectedFailure = e;
         randomSeed0 = rnd2; numSuccessShrinks = nss; numTryShrinks = nts;
         stDoAnalysis = ana }))
    maxSteps

(** val test : state -> (int -> randomSeed -> qProp) -> result0 **)

let test st f =
  if gte st.numSuccessTests st.maxSuccessTests
  then doneTesting st
  else if gte st.numDiscardedTests st.maxDiscardedTests
       then giveUp st
       else let maxSteps = add st.maxSuccessTests st.maxDiscardedTests in
            runATest st f maxSteps

(** val quickCheckWith : 'a1 checkable -> args -> 'a1 -> result0 **)

let quickCheckWith x a p =
  match a.replay with
  | Some p0 ->
    let (rnd, s) = p0 in
    let computeFun = at0 (computeSize' a) s in
    test { maxSuccessTests = a.maxSuccess; maxDiscardedTests = a.maxDiscard;
      maxShrinkNo = a.maxShrinks; computeSize = computeFun; numSuccessTests =
      0; numDiscardedTests = 0; labels = Map.empty; expectedFailure = false;
      randomSeed0 = rnd; numSuccessShrinks = 0; numTryShrinks = 0;
      stDoAnalysis = a.analysis } (run (x p))
  | None ->
    let computeFun = computeSize' a in
    test { maxSuccessTests = a.maxSuccess; maxDiscardedTests = a.maxDiscard;
      maxShrinkNo = a.maxShrinks; computeSize = computeFun; numSuccessTests =
      0; numDiscardedTests = 0; labels = Map.empty; expectedFailure = false;
      randomSeed0 = newRandomSeed; numSuccessShrinks = 0; numTryShrinks = 0;
      stDoAnalysis = a.analysis } (run (x p))

type oCamlFloat = float

(** val showOCamlFloat' : oCamlFloat -> char list **)

let showOCamlFloat' = (fun f -> Printf.sprintf "%.9fs" f |> String.to_seq |> List.of_seq)

(** val showOCamlFloat : oCamlFloat show **)

let showOCamlFloat =
  showOCamlFloat'

type 'a timedResult =
| TResult of 'a * oCamlFloat * oCamlFloat * oCamlFloat

(** val withTime : (unit -> 'a1) -> 'a1 timedResult **)

let withTime = 
  (fun f -> 
    let start = Unix.gettimeofday () in 
    let res = f () in 
    let ending = Unix.gettimeofday () in
    TResult (res, (ending -. start), start, ending))

(** val showTimedResult : 'a1 show -> 'a1 timedResult show **)

let showTimedResult h = function
| TResult (result2, time, _, _) ->
  append ('"'::('t'::('i'::('m'::('e'::('"'::(':'::(' '::('"'::[])))))))))
    (append (showOCamlFloat time)
      (append ('"'::(','::(' '::[]))) (h result2)))

(** val showCollectStatistics : (char list * int) list -> char list **)

let rec showCollectStatistics = function
| [] -> []
| p :: l' ->
  let (s, n0) = p in
  append (showNat n0)
    (append (' '::(':'::(' '::[])))
      (append s (append newline (showCollectStatistics l'))))

(** val showResult : result0 show **)

let showResult = function
| Success (_, _, l, s) -> append (showCollectStatistics l) s
| GaveUp (_, l, s) -> append (showCollectStatistics l) s
| Failure (_, _, _, _, _, s, l, _) -> append (showCollectStatistics l) s
| NoExpectedFailure (_, l, s) -> append (showCollectStatistics l) s

(** val print_extracted_coq_string : char list -> unit **)

let print_extracted_coq_string = fun l -> print_string (
   let s = Bytes.create (List.length l) in
   let rec copy i = function
    | [] -> s
    | c :: l -> Bytes.set s i c; copy (i+1) l
   in Bytes.to_string (copy 0 l))

type dec = decidable
  (* singleton inductive, whose constructor was Build_Dec *)

(** val checkable_opt : 'a1 checkable -> 'a1 option checkable **)

let checkable_opt h = function
| Some x -> h x
| None -> testUnit ()

type 'a dec_Eq =
  'a -> 'a -> decidable
  (* singleton inductive, whose constructor was Build_Dec_Eq *)

(** val dec_Eq_implies_DecEq : 'a1 dec_Eq -> 'a1 -> 'a1 -> dec **)

let dec_Eq_implies_DecEq h =
  h

(** val dec_eq_Z : Big_int_Z.big_int dec_Eq **)

let dec_eq_Z x y =
  (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
    (fun _ ->
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> true)
      (fun _ -> false)
      (fun _ -> false)
      y)
    (fun p ->
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> false)
      (fun p0 ->
      let rec f p1 x0 =
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun p3 -> f p2 p3)
            (fun _ -> false)
            (fun _ -> false)
            x0)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> false)
            (fun p3 -> f p2 p3)
            (fun _ -> false)
            x0)
          (fun _ ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> false)
            (fun _ -> false)
            (fun _ -> true)
            x0)
          p1
      in f p p0)
      (fun _ -> false)
      y)
    (fun p ->
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> false)
      (fun _ -> false)
      (fun p0 ->
      let rec f p1 x0 =
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun p3 -> f p2 p3)
            (fun _ -> false)
            (fun _ -> false)
            x0)
          (fun p2 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> false)
            (fun p3 -> f p2 p3)
            (fun _ -> false)
            x0)
          (fun _ ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> false)
            (fun _ -> false)
            (fun _ -> true)
            x0)
          p1
      in f p p0)
      y)
    x

(** val dec_eq_opt : 'a1 dec_Eq -> 'a1 option dec_Eq **)

let dec_eq_opt h x y =
  match x with
  | Some a -> (match y with
               | Some a0 -> h a a0
               | None -> false)
  | None -> (match y with
             | Some _ -> false
             | None -> true)

(** val dec_eq_prod : 'a1 dec_Eq -> 'a2 dec_Eq -> ('a1 * 'a2) dec_Eq **)

let dec_eq_prod h h0 x y =
  let (a, b) = x in let (a0, b0) = y in if h a a0 then h0 b b0 else false

(** val list_Dec_Eq : 'a1 dec_Eq -> 'a1 list dec_Eq **)

let rec list_Dec_Eq x0 x y =
  match x with
  | [] -> (match y with
           | [] -> true
           | _ :: _ -> false)
  | y0 :: l ->
    (match y with
     | [] -> false
     | x1 :: l0 -> if x0 y0 x1 then list_Dec_Eq x0 l l0 else false)

type ('a, 'b) implies = 'a -> 'b -> bool option

(** val implies0 : ('a1, 'a2) implies -> 'a1 -> 'a2 -> bool option **)

let implies0 implies1 =
  implies1

(** val impliesOO : (bool option, bool option) implies **)

let impliesOO p1 p2 =
  match p1 with
  | Some b -> if b then p2 else None
  | None -> None

(** val impliesBB : (bool, bool) implies **)

let impliesBB p1 p2 =
  impliesOO (Some p1) (Some p2)

(** val impliesBO : (bool, bool option) implies **)

let impliesBO p1 p2 =
  impliesOO (Some p1) p2

(** val genZSized : Big_int_Z.big_int genSized **)

let genZSized x =
  let z0 = Z.of_nat x in
  choose (Obj.magic producerGen) chooseZ ((Z.opp z0), z0)

(** val monad_option : __ option monad **)

let monad_option =
  { ret = (Obj.magic (fun _ x -> Some x)); bind = (fun _ _ c1 c2 ->
    match c1 with
    | Some v -> c2 v
    | None -> None) }

type color =
| R
| B

(** val showColor : color show **)

let showColor = function
| R -> 'R'::[]
| B -> 'B'::[]

type tree0 =
| E
| T of color * tree0 * Big_int_Z.big_int * Big_int_Z.big_int * tree0

(** val showTree : tree0 show **)

let rec showTree = function
| E -> '('::('E'::(')'::[]))
| T (c, l, k, v, r) ->
  append ('('::('T'::(' '::('('::[]))))
    (append (showColor c)
      (append (')'::(' '::[]))
        (append (showTree l)
          (append (' '::[])
            (append (showZ k)
              (append (' '::[])
                (append (showZ v)
                  (append (' '::[]) (append (showTree r) (')'::[]))))))))))

(** val fuel : int **)

let fuel = 100000

(** val blacken : tree0 -> tree0 **)

let blacken = function
| E -> E
| T (_, a, x, vx, b) -> T (B, a, x, vx, b)

(** val redden : tree0 -> tree0 option **)

let redden = function
| E -> None
| T (c, a, x, vx, b) ->
  (match c with
   | R -> None
   | B -> Some (T (R, a, x, vx, b)))

(** val balance :
    color -> tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 **)

let balance col tl key0 val0 tr =
  match col with
  | R -> T (col, tl, key0, val0, tr)
  | B ->
    (match tl with
     | E ->
       (match tr with
        | E -> T (col, tl, key0, val0, tr)
        | T (c0, b, y, vy, d) ->
          (match c0 with
           | R ->
             (match b with
              | E ->
                (match d with
                 | E -> T (col, tl, key0, val0, tr)
                 | T (c1, c, z0, vz, d0) ->
                   (match c1 with
                    | R ->
                      T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c, z0,
                        vz, d0)))
                    | B -> T (col, tl, key0, val0, tr)))
              | T (c1, b0, y0, vy0, c) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T (B, c, y,
                     vy, d)))
                 | B ->
                   (match d with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key0, val0, tr)))))
           | B -> T (col, tl, key0, val0, tr)))
     | T (c0, a, x, vx, c) ->
       (match c0 with
        | R ->
          (match a with
           | E ->
             (match c with
              | E ->
                (match tr with
                 | E -> T (col, tl, key0, val0, tr)
                 | T (c1, b, y, vy, d) ->
                   (match c1 with
                    | R ->
                      (match b with
                       | E ->
                         (match d with
                          | E -> T (col, tl, key0, val0, tr)
                          | T (c2, c3, z0, vz, d0) ->
                            (match c2 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b)), y, vy, (T
                                 (B, c3, z0, vz, d0)))
                             | B -> T (col, tl, key0, val0, tr)))
                       | T (c2, b0, y0, vy0, c3) ->
                         (match c2 with
                          | R ->
                            T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T
                              (B, c3, y, vy, d)))
                          | B ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b)), y, vy, (T
                                    (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))))
                    | B -> T (col, tl, key0, val0, tr)))
              | T (c1, b, y, vy, c2) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, a, x, vx, b)), y, vy, (T (B, c2, key0, val0,
                     tr)))
                 | B ->
                   (match tr with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c3, b0, y0, vy0, d) ->
                      (match c3 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b0)), y0, vy0,
                                    (T (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))
                          | T (c4, b1, y1, vy1, c5) ->
                            (match c4 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b1)), y1, vy1, (T
                                 (B, c5, y0, vy0, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c6, c7, z0, vz, d0) ->
                                  (match c6 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b0)), y0,
                                       vy0, (T (B, c7, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))))
                       | B -> T (col, tl, key0, val0, tr)))))
           | T (c1, a0, x0, vx0, b) ->
             (match c1 with
              | R ->
                T (R, (T (B, a0, x0, vx0, b)), x, vx, (T (B, c, key0, val0,
                  tr)))
              | B ->
                (match c with
                 | E ->
                   (match tr with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, b0, y, vy, d) ->
                      (match c2 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c3, c4, z0, vz, d0) ->
                               (match c3 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b0)), y, vy,
                                    (T (B, c4, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))
                          | T (c3, b1, y0, vy0, c4) ->
                            (match c3 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b1)), y0, vy0, (T
                                 (B, c4, y, vy, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b0)), y,
                                       vy, (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))))
                       | B -> T (col, tl, key0, val0, tr)))
                 | T (c2, b0, y, vy, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, a, x, vx, b0)), y, vy, (T (B, c3, key0,
                        val0, tr)))
                    | B ->
                      (match tr with
                       | E -> T (col, tl, key0, val0, tr)
                       | T (c4, b1, y0, vy0, d) ->
                         (match c4 with
                          | R ->
                            (match b1 with
                             | E ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b1)), y0,
                                       vy0, (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))
                             | T (c5, b2, y1, vy1, c6) ->
                               (match c5 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b2)), y1, vy1,
                                    (T (B, c6, y0, vy0, d)))
                                | B ->
                                  (match d with
                                   | E -> T (col, tl, key0, val0, tr)
                                   | T (c7, c8, z0, vz, d0) ->
                                     (match c7 with
                                      | R ->
                                        T (R, (T (B, tl, key0, val0, b1)),
                                          y0, vy0, (T (B, c8, z0, vz, d0)))
                                      | B -> T (col, tl, key0, val0, tr)))))
                          | B -> T (col, tl, key0, val0, tr)))))))
        | B ->
          (match tr with
           | E -> T (col, tl, key0, val0, tr)
           | T (c1, b, y, vy, d) ->
             (match c1 with
              | R ->
                (match b with
                 | E ->
                   (match d with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key0, val0, tr)))
                 | T (c2, b0, y0, vy0, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T (B, c3,
                        y, vy, d)))
                    | B ->
                      (match d with
                       | E -> T (col, tl, key0, val0, tr)
                       | T (c4, c5, z0, vz, d0) ->
                         (match c4 with
                          | R ->
                            T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B,
                              c5, z0, vz, d0)))
                          | B -> T (col, tl, key0, val0, tr)))))
              | B -> T (col, tl, key0, val0, tr)))))

(** val insert : Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 **)

let rec insert key0 val0 t0 =
  let ins =
    let rec ins x vx = function
    | E -> T (B, E, x, vx, E)
    | T (rb, a, y, vy, b) ->
      if z_lt_le_dec x y
      then balance rb (ins x vx a) y vy b
      else if z_lt_le_dec y x
           then balance rb a y vy (ins x vx b)
           else T (rb, a, y, vx, b)
    in ins
  in
  blacken (ins key0 val0 t0)

(** val balLeft :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 option **)

let balLeft tl k v tr =
  match tl with
  | E ->
    (match tr with
     | E -> None
     | T (c0, a, z0, vz, c) ->
       (match c0 with
        | R ->
          (match a with
           | E -> None
           | T (c1, a0, y, vy, b) ->
             (match c1 with
              | R -> None
              | B ->
                bind (Obj.magic monad_option) (redden c) (fun c' -> Some (T
                  (R, (T (B, tl, k, v, a0)), y, vy, (balance B b z0 vz c'))))))
        | B -> Some (balance B tl k v (T (R, a, z0, vz, c)))))
  | T (c0, a, x, vx, b) ->
    (match c0 with
     | R -> Some (T (R, (T (B, a, x, vx, b)), k, v, tr))
     | B ->
       (match tr with
        | E -> None
        | T (c1, a0, z0, vz, c) ->
          (match c1 with
           | R ->
             (match a0 with
              | E -> None
              | T (c2, a1, y, vy, b0) ->
                (match c2 with
                 | R -> None
                 | B ->
                   bind (Obj.magic monad_option) (redden c) (fun c' -> Some
                     (T (R, (T (B, tl, k, v, a1)), y, vy,
                     (balance B b0 z0 vz c'))))))
           | B -> Some (balance B tl k v (T (R, a0, z0, vz, c))))))

(** val balRight :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 option **)

let balRight tl k v tr =
  match tl with
  | E ->
    (match tr with
     | E -> None
     | T (c0, b, y, vy, c) ->
       (match c0 with
        | R -> Some (T (R, tl, k, v, (T (B, b, y, vy, c))))
        | B -> None))
  | T (c0, a, x, vx, b) ->
    (match c0 with
     | R ->
       (match b with
        | E ->
          (match tr with
           | E -> None
           | T (c1, b0, y, vy, c) ->
             (match c1 with
              | R -> Some (T (R, tl, k, v, (T (B, b0, y, vy, c))))
              | B -> None))
        | T (c1, b0, y, vy, c) ->
          (match c1 with
           | R ->
             (match tr with
              | E -> None
              | T (c2, b1, y0, vy0, c3) ->
                (match c2 with
                 | R -> Some (T (R, tl, k, v, (T (B, b1, y0, vy0, c3))))
                 | B -> None))
           | B ->
             (match tr with
              | E ->
                bind (Obj.magic monad_option) (redden a) (fun a' -> Some (T
                  (R, (balance B a' x vx b0), y, vy, (T (B, c, k, v, tr)))))
              | T (c2, b1, y0, vy0, c3) ->
                (match c2 with
                 | R -> Some (T (R, tl, k, v, (T (B, b1, y0, vy0, c3))))
                 | B ->
                   bind (Obj.magic monad_option) (redden a) (fun a' -> Some
                     (T (R, (balance B a' x vx b0), y, vy, (T (B, c, k, v,
                     tr)))))))))
     | B ->
       (match tr with
        | E -> Some (balance B (T (R, a, x, vx, b)) k v tr)
        | T (c1, b0, y, vy, c) ->
          (match c1 with
           | R -> Some (T (R, tl, k, v, (T (B, b0, y, vy, c))))
           | B -> Some (balance B (T (R, a, x, vx, b)) k v tr))))

(** val _join : tree0 -> tree0 -> int -> tree0 option **)

let rec _join t1 t2 f =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> None)
    (fun f' ->
    match t1 with
    | E -> Some t2
    | T (c0, a, x, vx, b) ->
      (match c0 with
       | R ->
         (match t2 with
          | E -> Some t1
          | T (c1, b0, x0, vx0, c) ->
            (match c1 with
             | R ->
               (match _join b b0 f' with
                | Some bc ->
                  (match bc with
                   | E -> Some (T (R, a, x, vx, (T (R, bc, x0, vx0, c))))
                   | T (c2, b', z0, vz, c') ->
                     (match c2 with
                      | R ->
                        Some (T (R, (T (R, a, x, vx, b')), z0, vz, (T (R, c',
                          x0, vx0, c))))
                      | B -> Some (T (R, a, x, vx, (T (R, bc, x0, vx0, c))))))
                | None -> None)
             | B ->
               bind (Obj.magic monad_option) (_join b t2 f') (fun t' -> Some
                 (T (R, a, x, vx, t')))))
       | B ->
         (match t2 with
          | E -> Some t1
          | T (c1, b0, x0, vx0, c) ->
            (match c1 with
             | R ->
               (match _join t1 b0 f' with
                | Some t' -> Some (T (R, t', x0, vx0, c))
                | None -> None)
             | B ->
               (match _join b b0 f' with
                | Some bc ->
                  (match bc with
                   | E -> balLeft a x vx (T (B, bc, x0, vx0, c))
                   | T (c2, b', z0, vz, c') ->
                     (match c2 with
                      | R ->
                        Some (T (R, (T (B, a, x, vx, b')), z0, vz, (T (B, c',
                          x0, vx0, c))))
                      | B -> balLeft a x vx (T (B, bc, x0, vx0, c))))
                | None -> None)))))
    f

(** val join0 : tree0 -> tree0 -> tree0 option **)

let join0 t1 t2 =
  _join t1 t2 fuel

(** val del : Big_int_Z.big_int -> tree0 -> int -> tree0 option **)

let rec del x s f =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> None)
    (fun f' ->
    match s with
    | E -> Some E
    | T (_, a, y, vy, b) ->
      if z_lt_le_dec x y
      then bind (Obj.magic monad_option) (delLeft x a y vy b f') (fun t' ->
             Some t')
      else if z_lt_le_dec y x
           then bind (Obj.magic monad_option) (delRight x a y vy b f')
                  (fun t' -> Some t')
           else bind (Obj.magic monad_option) (join0 a b) (fun t' -> Some t'))
    f

(** val delLeft :
    Big_int_Z.big_int -> tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    tree0 -> int -> tree0 option **)

and delLeft x dl dy dvy dr f =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> None)
    (fun f' ->
    match dl with
    | E ->
      bind (Obj.magic monad_option) (del x dl f') (fun t' -> Some (T (R, t',
        dy, dvy, dr)))
    | T (c, al, ax, avx, ar) ->
      (match c with
       | R ->
         bind (Obj.magic monad_option) (del x dl f') (fun t' -> Some (T (R,
           t', dy, dvy, dr)))
       | B ->
         bind (Obj.magic monad_option) (del x (T (B, al, ax, avx, ar)) f')
           (fun t' ->
           bind (Obj.magic monad_option) (balLeft t' dy dvy dr) (fun t'' ->
             Some t''))))
    f

(** val delRight :
    Big_int_Z.big_int -> tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    tree0 -> int -> tree0 option **)

and delRight x dl dy dvy dr f =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> None)
    (fun f' ->
    match dr with
    | E ->
      bind (Obj.magic monad_option) (del x dr f') (fun t' -> Some (T (R, dl,
        dy, dvy, t')))
    | T (c, bl, bx, bvx, br) ->
      (match c with
       | R ->
         bind (Obj.magic monad_option) (del x dr f') (fun t' -> Some (T (R,
           dl, dy, dvy, t')))
       | B ->
         bind (Obj.magic monad_option) (del x (T (B, bl, bx, bvx, br)) f')
           (fun t' ->
           bind (Obj.magic monad_option) (balRight dl dy dvy t') (fun t'' ->
             Some t''))))
    f

(** val delete : Big_int_Z.big_int -> tree0 -> tree0 option **)

let delete x t0 =
  bind (Obj.magic monad_option) (del x t0 fuel) (fun t' -> Some (blacken t'))

(** val find0 : Big_int_Z.big_int -> tree0 -> Big_int_Z.big_int option **)

let rec find0 x = function
| E -> None
| T (_, l, y, vy, r) ->
  if z_lt_le_dec x y
  then find0 x l
  else if z_lt_le_dec y x then find0 x r else Some vy

(** val isBST : tree0 -> bool **)

let rec isBST t0 =
  let every =
    let rec every p = function
    | E -> true
    | T (_, a, x, _, b) -> (&&) ((&&) (p x) (every p a)) (every p b)
    in every
  in
  (match t0 with
   | E -> true
   | T (_, a, x, _, b) ->
     (&&) ((&&) ((&&) (every (Z.gtb x) a) (every (Z.ltb x) b)) (isBST a))
       (isBST b))

(** val noRedRed : tree0 -> bool **)

let rec noRedRed t0 =
  let blackRoot = fun t1 ->
    match t1 with
    | E -> true
    | T (c, _, _, _, _) -> (match c with
                            | R -> false
                            | B -> true)
  in
  (match t0 with
   | E -> true
   | T (c, a, _, _, b) ->
     (match c with
      | R ->
        (&&) ((&&) ((&&) (blackRoot a) (blackRoot b)) (noRedRed a))
          (noRedRed b)
      | B -> (&&) (noRedRed a) (noRedRed b)))

(** val consistentBlackHeight : tree0 -> bool **)

let consistentBlackHeight t0 =
  let go =
    let rec go = function
    | E -> (true, Big_int_Z.unit_big_int)
    | T (rb, a, _, _, b) ->
      let (aBool, aHeight) = go a in
      let (bBool, bHeight) = go b in
      let isBlack = fun rb0 ->
        match rb0 with
        | R -> Big_int_Z.zero_big_int
        | B -> Big_int_Z.unit_big_int
      in
      (((&&) ((&&) aBool bBool) (Z.eqb aHeight bHeight)),
      (Z.add aHeight (isBlack rb)))
    in go
  in
  fst (go t0)

(** val isRBT : tree0 -> bool **)

let isRBT t0 =
  (&&) ((&&) (isBST t0) (consistentBlackHeight t0)) (noRedRed t0)

(** val toList : tree0 -> (Big_int_Z.big_int * Big_int_Z.big_int) list **)

let rec toList = function
| E -> []
| T (_, l, k, v, r) -> app (toList l) (app ((k, v) :: []) (toList r))

(** val prop_InsertValid :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> bool option **)

let prop_InsertValid t0 k v =
  implies0 impliesBB (isRBT t0) (isRBT (insert k v t0))

(** val prop_DeleteValid : tree0 -> Big_int_Z.big_int -> bool option **)

let prop_DeleteValid t0 k =
  implies0 impliesBO (isRBT t0)
    (match delete k t0 with
     | Some t' -> Some (isRBT t')
     | None -> Some false)

(** val prop_InsertPost :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    bool option **)

let prop_InsertPost t0 k k' v =
  implies0 impliesBB (isRBT t0)
    (let v' = find0 k' (insert k v t0) in
     if Z.eqb k k'
     then if dec_Eq_implies_DecEq (dec_eq_opt dec_eq_Z) v' (Some v)
          then true
          else false
     else if dec_Eq_implies_DecEq (dec_eq_opt dec_eq_Z) v' (find0 k' t0)
          then true
          else false)

(** val prop_DeletePost :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> bool option **)

let prop_DeletePost t0 k k' =
  implies0 impliesBB (isRBT t0)
    (match delete k t0 with
     | Some t' ->
       if dec_Eq_implies_DecEq (dec_eq_opt dec_eq_Z) (find0 k' t')
            (if Z.eqb k k' then None else find0 k' t0)
       then true
       else false
     | None -> false)

(** val deleteKey :
    Big_int_Z.big_int -> (Big_int_Z.big_int * Big_int_Z.big_int) list ->
    (Big_int_Z.big_int * Big_int_Z.big_int) list **)

let deleteKey k l =
  filter (fun x -> negb (Z.eqb (fst x) k)) l

(** val l_insert :
    (Big_int_Z.big_int * Big_int_Z.big_int) ->
    (Big_int_Z.big_int * Big_int_Z.big_int) list ->
    (Big_int_Z.big_int * Big_int_Z.big_int) list **)

let rec l_insert kv l = match l with
| [] -> kv :: []
| p :: xs ->
  let (k, v) = p in
  if Z.eqb (fst kv) k
  then ((fst kv), (snd kv)) :: xs
  else if Z.ltb (fst kv) k
       then ((fst kv), (snd kv)) :: l
       else (k, v) :: (l_insert kv xs)

(** val prop_InsertModel :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> bool option **)

let prop_InsertModel t0 k v =
  implies0 impliesBB (isRBT t0)
    (if dec_Eq_implies_DecEq (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z))
          (toList (insert k v t0)) (l_insert (k, v) (deleteKey k (toList t0)))
     then true
     else false)

(** val prop_DeleteModel : tree0 -> Big_int_Z.big_int -> bool option **)

let prop_DeleteModel t0 k =
  implies0 impliesBB (isRBT t0)
    (match delete k t0 with
     | Some t' ->
       if dec_Eq_implies_DecEq (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z))
            (toList t') (deleteKey k (toList t0))
       then true
       else false
     | None -> false)

(** val prop_InsertInsert :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> bool option **)

let prop_InsertInsert t0 k k' v v' =
  implies0 impliesBB (isRBT t0)
    (if dec_Eq_implies_DecEq (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z))
          (toList (insert k v (insert k' v' t0)))
          (toList
            (if Z.eqb k k'
             then insert k v t0
             else insert k' v' (insert k v t0)))
     then true
     else false)

(** val prop_InsertDelete :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    bool option **)

let prop_InsertDelete t0 k k' v =
  implies0 impliesBB (isRBT t0)
    (match delete k' t0 with
     | Some t' ->
       (match delete k' (insert k v t0) with
        | Some t'' ->
          if dec_Eq_implies_DecEq
               (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z))
               (toList (insert k v t'))
               (toList (if Z.eqb k k' then insert k v t0 else t''))
          then true
          else false
        | None -> false)
     | None -> false)

(** val prop_DeleteInsert :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    bool option **)

let prop_DeleteInsert t0 k k' v' =
  implies0 impliesBB (isRBT t0)
    (match delete k (insert k' v' t0) with
     | Some t' ->
       (match delete k t0 with
        | Some t'' ->
          let t''' = insert k' v' t'' in
          if dec_Eq_implies_DecEq
               (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z)) (toList t')
               (toList (if Z.eqb k k' then t'' else t'''))
          then true
          else false
        | None -> false)
     | None -> false)

(** val prop_DeleteDelete :
    tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> bool option **)

let prop_DeleteDelete t0 k k' =
  implies0 impliesBB (isRBT t0)
    (match delete k' t0 with
     | Some t' ->
       (match delete k t' with
        | Some t'' ->
          (match delete k t0 with
           | Some t1' ->
             (match delete k' t1' with
              | Some t1'' ->
                if dec_Eq_implies_DecEq
                     (list_Dec_Eq (dec_eq_prod dec_eq_Z dec_eq_Z))
                     (toList t'') (toList t1'')
                then true
                else false
              | None -> false)
           | None -> false)
        | None -> false)
     | None -> false)

(** val genZ : Big_int_Z.big_int g **)

let genZ =
  choose (Obj.magic producerGen) chooseZ ((Big_int_Z.minus_big_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))),
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))

(** val gen_kvs : int -> (Big_int_Z.big_int * Big_int_Z.big_int) list g **)

let rec gen_kvs s =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ret (Obj.magic monadGen) [])
    (fun s' ->
    bind (Obj.magic monadGen) (Obj.magic genZ) (fun k ->
      bind (Obj.magic monadGen) (genOfGenSized (Obj.magic genZSized))
        (fun v ->
        bind (Obj.magic monadGen) (gen_kvs s') (fun kvs ->
          ret (Obj.magic monadGen) ((k, v) :: kvs)))))
    s

(** val blacken_ : tree0 -> tree0 **)

let blacken_ = function
| E -> E
| T (_, a, x, vx, b) -> T (B, a, x, vx, b)

(** val balance_ :
    color -> tree0 -> Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 **)

let balance_ col tl key0 val0 tr =
  match col with
  | R -> T (col, tl, key0, val0, tr)
  | B ->
    (match tl with
     | E ->
       (match tr with
        | E -> T (col, tl, key0, val0, tr)
        | T (c0, b, y, vy, d) ->
          (match c0 with
           | R ->
             (match b with
              | E ->
                (match d with
                 | E -> T (col, tl, key0, val0, tr)
                 | T (c1, c, z0, vz, d0) ->
                   (match c1 with
                    | R ->
                      T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c, z0,
                        vz, d0)))
                    | B -> T (col, tl, key0, val0, tr)))
              | T (c1, b0, y0, vy0, c) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T (B, c, y,
                     vy, d)))
                 | B ->
                   (match d with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key0, val0, tr)))))
           | B -> T (col, tl, key0, val0, tr)))
     | T (c0, a, x, vx, c) ->
       (match c0 with
        | R ->
          (match a with
           | E ->
             (match c with
              | E ->
                (match tr with
                 | E -> T (col, tl, key0, val0, tr)
                 | T (c1, b, y, vy, d) ->
                   (match c1 with
                    | R ->
                      (match b with
                       | E ->
                         (match d with
                          | E -> T (col, tl, key0, val0, tr)
                          | T (c2, c3, z0, vz, d0) ->
                            (match c2 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b)), y, vy, (T
                                 (B, c3, z0, vz, d0)))
                             | B -> T (col, tl, key0, val0, tr)))
                       | T (c2, b0, y0, vy0, c3) ->
                         (match c2 with
                          | R ->
                            T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T
                              (B, c3, y, vy, d)))
                          | B ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b)), y, vy, (T
                                    (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))))
                    | B -> T (col, tl, key0, val0, tr)))
              | T (c1, b, y, vy, c2) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, a, x, vx, b)), y, vy, (T (B, c2, key0, val0,
                     tr)))
                 | B ->
                   (match tr with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c3, b0, y0, vy0, d) ->
                      (match c3 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b0)), y0, vy0,
                                    (T (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))
                          | T (c4, b1, y1, vy1, c5) ->
                            (match c4 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b1)), y1, vy1, (T
                                 (B, c5, y0, vy0, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c6, c7, z0, vz, d0) ->
                                  (match c6 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b0)), y0,
                                       vy0, (T (B, c7, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))))
                       | B -> T (col, tl, key0, val0, tr)))))
           | T (c1, a0, x0, vx0, b) ->
             (match c1 with
              | R ->
                T (R, (T (B, a0, x0, vx0, b)), x, vx, (T (B, c, key0, val0,
                  tr)))
              | B ->
                (match c with
                 | E ->
                   (match tr with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, b0, y, vy, d) ->
                      (match c2 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key0, val0, tr)
                             | T (c3, c4, z0, vz, d0) ->
                               (match c3 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b0)), y, vy,
                                    (T (B, c4, z0, vz, d0)))
                                | B -> T (col, tl, key0, val0, tr)))
                          | T (c3, b1, y0, vy0, c4) ->
                            (match c3 with
                             | R ->
                               T (R, (T (B, tl, key0, val0, b1)), y0, vy0, (T
                                 (B, c4, y, vy, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b0)), y,
                                       vy, (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))))
                       | B -> T (col, tl, key0, val0, tr)))
                 | T (c2, b0, y, vy, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, a, x, vx, b0)), y, vy, (T (B, c3, key0,
                        val0, tr)))
                    | B ->
                      (match tr with
                       | E -> T (col, tl, key0, val0, tr)
                       | T (c4, b1, y0, vy0, d) ->
                         (match c4 with
                          | R ->
                            (match b1 with
                             | E ->
                               (match d with
                                | E -> T (col, tl, key0, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key0, val0, b1)), y0,
                                       vy0, (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key0, val0, tr)))
                             | T (c5, b2, y1, vy1, c6) ->
                               (match c5 with
                                | R ->
                                  T (R, (T (B, tl, key0, val0, b2)), y1, vy1,
                                    (T (B, c6, y0, vy0, d)))
                                | B ->
                                  (match d with
                                   | E -> T (col, tl, key0, val0, tr)
                                   | T (c7, c8, z0, vz, d0) ->
                                     (match c7 with
                                      | R ->
                                        T (R, (T (B, tl, key0, val0, b1)),
                                          y0, vy0, (T (B, c8, z0, vz, d0)))
                                      | B -> T (col, tl, key0, val0, tr)))))
                          | B -> T (col, tl, key0, val0, tr)))))))
        | B ->
          (match tr with
           | E -> T (col, tl, key0, val0, tr)
           | T (c1, b, y, vy, d) ->
             (match c1 with
              | R ->
                (match b with
                 | E ->
                   (match d with
                    | E -> T (col, tl, key0, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key0, val0, tr)))
                 | T (c2, b0, y0, vy0, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, tl, key0, val0, b0)), y0, vy0, (T (B, c3,
                        y, vy, d)))
                    | B ->
                      (match d with
                       | E -> T (col, tl, key0, val0, tr)
                       | T (c4, c5, z0, vz, d0) ->
                         (match c4 with
                          | R ->
                            T (R, (T (B, tl, key0, val0, b)), y, vy, (T (B,
                              c5, z0, vz, d0)))
                          | B -> T (col, tl, key0, val0, tr)))))
              | B -> T (col, tl, key0, val0, tr)))))

(** val insert_ : Big_int_Z.big_int -> Big_int_Z.big_int -> tree0 -> tree0 **)

let rec insert_ key0 val0 t0 =
  let ins =
    let rec ins x vx = function
    | E -> T (R, E, x, vx, E)
    | T (rb, a, y, vy, b) ->
      if z_lt_le_dec x y
      then balance_ rb (ins x vx a) y vy b
      else if z_lt_le_dec y x
           then balance_ rb a y vy (ins x vx b)
           else T (rb, a, y, vx, b)
    in ins
  in
  blacken_ (ins key0 val0 t0)

(** val gen_tree : int -> tree0 g **)

let gen_tree s =
  bind (Obj.magic monadGen)
    (choose (Obj.magic producerGen) chooseNat ((Stdlib.Int.succ 0), s))
    (fun sz ->
    bind (Obj.magic monadGen) (Obj.magic gen_kvs sz) (fun kvs ->
      ret (Obj.magic monadGen)
        (fold_right (fun pat t0 -> let (k, v) = pat in insert_ k v t0) E kvs)))

(** val bespoke : tree0 g **)

let bespoke =
  gen_tree (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))

(** val test_prop_InsertValid : checker **)

let test_prop_InsertValid =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
        (fun v -> prop_InsertValid t0 k v)))

(** val test_prop_DeleteValid : checker **)

let test_prop_DeleteValid =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll (checkable_opt testBool) showZ genZ (fun k ->
      prop_DeleteValid t0 k))

(** val test_prop_InsertPost : checker **)

let test_prop_InsertPost =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll testChecker showZ genZ (fun k' ->
        forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
          (fun v -> prop_InsertPost t0 k k' v))))

(** val test_prop_DeletePost : checker **)

let test_prop_DeletePost =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll (checkable_opt testBool) showZ genZ (fun k' ->
        prop_DeletePost t0 k k')))

(** val test_prop_InsertModel : checker **)

let test_prop_InsertModel =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
        (fun v -> prop_InsertModel t0 k v)))

(** val test_prop_DeleteModel : checker **)

let test_prop_DeleteModel =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll (checkable_opt testBool) showZ genZ (fun k ->
      prop_DeleteModel t0 k))

(** val test_prop_InsertInsert : checker **)

let test_prop_InsertInsert =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll testChecker showZ genZ (fun k' ->
        forAll testChecker showZ (genOfGenSized genZSized) (fun v ->
          forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
            (fun v' -> prop_InsertInsert t0 k k' v v')))))

(** val test_prop_InsertDelete : checker **)

let test_prop_InsertDelete =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll testChecker showZ genZ (fun k' ->
        forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
          (fun v -> prop_InsertDelete t0 k k' v))))

(** val test_prop_DeleteInsert : checker **)

let test_prop_DeleteInsert =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll testChecker showZ genZ (fun k' ->
        forAll (checkable_opt testBool) showZ (genOfGenSized genZSized)
          (fun v' -> prop_DeleteInsert t0 k k' v'))))

(** val test_prop_DeleteDelete : checker **)

let test_prop_DeleteDelete =
  forAll testChecker showTree bespoke (fun t0 ->
    forAll testChecker showZ genZ (fun k ->
      forAll (checkable_opt testBool) showZ genZ (fun k' ->
        prop_DeleteDelete t0 k k')))

(** val num_tests : int **)

let num_tests = max_int

(** val qctest_test_prop_InsertValid : unit -> unit **)

let qctest_test_prop_InsertValid _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_InsertValid))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_DeleteValid : unit -> unit **)

let qctest_test_prop_DeleteValid _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_DeleteValid))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_InsertPost : unit -> unit **)

let qctest_test_prop_InsertPost _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_InsertPost))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_DeletePost : unit -> unit **)

let qctest_test_prop_DeletePost _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_DeletePost))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_InsertModel : unit -> unit **)

let qctest_test_prop_InsertModel _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_InsertModel))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_DeleteModel : unit -> unit **)

let qctest_test_prop_DeleteModel _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_DeleteModel))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_InsertInsert : unit -> unit **)

let qctest_test_prop_InsertInsert _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_InsertInsert))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_InsertDelete : unit -> unit **)

let qctest_test_prop_InsertDelete _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_InsertDelete))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_DeleteInsert : unit -> unit **)

let qctest_test_prop_DeleteInsert _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_DeleteInsert))) ('}'::('|'::(']'::[])))))

(** val qctest_test_prop_DeleteDelete : unit -> unit **)

let qctest_test_prop_DeleteDelete _ =
  print_extracted_coq_string
    (append ('['::('|'::('{'::[])))
      (append
        (showTimedResult showResult
          (withTime (fun _ ->
            quickCheckWith testChecker
              (updMaxDiscard
                (updMaxSuccess (updAnalysis stdArgs true) num_tests)
                num_tests) test_prop_DeleteDelete))) ('}'::('|'::(']'::[])))))

type oCamlString = string

(** val qctest_map : oCamlString -> unit **)

let qctest_map = 
fun test_name ->
  let test_map = [
    ("InsertValid", qctest_test_prop_InsertValid);
    ("DeleteValid", qctest_test_prop_DeleteValid);
    ("InsertPost", qctest_test_prop_InsertPost);
    ("DeletePost", qctest_test_prop_DeletePost);
    ("InsertModel", qctest_test_prop_InsertModel);
    ("DeleteModel", qctest_test_prop_DeleteModel);
    ("InsertInsert", qctest_test_prop_InsertInsert);
    ("InsertDelete", qctest_test_prop_InsertDelete);
    ("DeleteInsert", qctest_test_prop_DeleteInsert);
    ("DeleteDelete", qctest_test_prop_DeleteDelete)
  ] in
  let test = List.assoc test_name test_map in
  test ()


let () =
Sys.argv.(1) |> qctest_map


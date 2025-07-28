
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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
  type mask =
  | IsNul
  | IsPos of Big_int_Z.big_int
  | IsNeg
 end

module Coq_Pos =
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

  type mask = Pos.mask =
  | IsNul
  | IsPos of Big_int_Z.big_int
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos Big_int_Z.unit_big_int
  | IsPos p ->
    IsPos ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (Big_int_Z.mult_int_big_int 2 p)
  | x0 -> x0

  (** val double_pred_mask : Big_int_Z.big_int -> mask **)

  let double_pred_mask x =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p -> IsPos (Big_int_Z.mult_int_big_int 2
      (Big_int_Z.mult_int_big_int 2 p)))
      (fun p -> IsPos (Big_int_Z.mult_int_big_int 2
      (pred_double p)))
      (fun _ -> IsNul)
      x

  (** val sub_mask : Big_int_Z.big_int -> Big_int_Z.big_int -> mask **)

  let rec sub_mask x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> double_mask (sub_mask p q))
        (fun q -> succ_double_mask (sub_mask p q))
        (fun _ -> IsPos (Big_int_Z.mult_int_big_int 2 p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun q -> double_mask (sub_mask p q))
        (fun _ -> IsPos (pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> IsNeg)
        (fun _ -> IsNeg)
        (fun _ -> IsNul)
        y)
      x

  (** val sub_mask_carry : Big_int_Z.big_int -> Big_int_Z.big_int -> mask **)

  and sub_mask_carry x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun q -> double_mask (sub_mask p q))
        (fun _ -> IsPos (pred_double p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> double_mask (sub_mask_carry p q))
        (fun q -> succ_double_mask (sub_mask_carry p q))
        (fun _ -> double_pred_mask p)
        y)
      (fun _ -> IsNeg)
      x

  (** val compare_cont :
      comparison -> Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let rec compare_cont = (fun c x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then c else if s < 0 then Lt else Gt)

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

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
 end

module N =
 struct
  (** val succ : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let succ = Big_int_Z.succ_big_int

  (** val sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sub = (fun n m -> Big_int_Z.max_big_int Big_int_Z.zero_big_int
  (Big_int_Z.sub_big_int n m))

  (** val to_nat : Big_int_Z.big_int -> int **)

  let to_nat a =
    (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
      (fun _ -> 0)
      (fun p -> Coq_Pos.to_nat p)
      a

  (** val of_nat : int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n' -> (Coq_Pos.of_succ_nat n'))
      n0
 end

(** val rev0 : 'a1 list -> 'a1 list **)

let rec rev0 = function
| [] -> []
| x :: l' -> app (rev0 l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t -> f b (fold_right f a0 t)

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
      (fun p -> Big_int_Z.minus_big_int (Coq_Pos.pred_double p))
      x

  (** val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let pred_double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.minus_big_int Big_int_Z.unit_big_int)
      (fun p -> (Coq_Pos.pred_double p))
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
        (fun _ -> (Coq_Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2
        q))
        (fun q -> Big_int_Z.minus_big_int (Coq_Pos.pred_double q))
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

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val to_nat : Big_int_Z.big_int -> int **)

  let to_nat z0 =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> 0)
      (fun p -> Coq_Pos.to_nat p)
      (fun _ -> 0)
      z0

  (** val of_nat : int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n1 -> (Coq_Pos.of_succ_nat n1))
      n0

  (** val of_N : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let of_N = (fun p -> p)

  (** val to_int :
      Big_int_Z.big_int ->
      ((Obj.t -> Obj.t) -> (Obj.t -> Obj.t) -> Obj.t) (* Decimal.int *) **)

  let to_int n0 =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> (fun x pos _ -> pos (Obj.magic x)) (D0 Nil))
      (fun p -> (fun x pos _ -> pos (Obj.magic x))
      (Coq_Pos.to_uint p))
      (fun p -> (fun y _ neg -> neg (Obj.magic y)) (Coq_Pos.to_uint p))
      n0
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

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

(** val concat : char list -> char list list -> char list **)

let rec concat sep = function
| [] -> []
| x :: xs ->
  (match xs with
   | [] -> x
   | _ :: _ -> append x (append sep (concat sep xs)))

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

(** val show_N : Big_int_Z.big_int -> char list **)

let show_N = (fun i ->
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

(** val showN : Big_int_Z.big_int show **)

let showN =
  show_N

type 'm monad = { ret : (__ -> __ -> 'm);
                  bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

(** val ret : 'a1 monad -> 'a2 -> 'a1 **)

let ret monad0 x =
  Obj.magic monad0.ret __ x

(** val bind : 'a1 monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad0 x x0 =
  Obj.magic monad0.bind __ __ x x0

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

(** val randomRN :
    (Big_int_Z.big_int * Big_int_Z.big_int) -> randomSeed ->
    Big_int_Z.big_int * randomSeed **)

let randomRN = (fun (x,y) r ->
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

(** val enumRN :
    (Big_int_Z.big_int * Big_int_Z.big_int) -> Big_int_Z.big_int lazyList **)

let enumRN p =
  lazy_seq N.succ (fst p) (Stdlib.Int.succ (N.to_nat (N.sub (snd p) (fst p))))

(** val chooseN : Big_int_Z.big_int choosableFromInterval **)

let chooseN =
  { randomR = randomRN; enumR = enumRN }

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

type 'a genSized =
  int -> 'a g
  (* singleton inductive, whose constructor was Build_GenSized *)

type 'a gen = 'a g
  (* singleton inductive, whose constructor was Build_Gen *)

(** val genOfGenSized : 'a1 genSized -> 'a1 gen **)

let genOfGenSized h =
  sized (Obj.magic producerGen) h

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

(** val quickSample : 'a1 show -> args -> 'a1 g -> 'a1 timedResult list **)

let quickSample _ a g0 =
  let rec aux n0 cnt acc rnd =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> rev0 acc)
      (fun n' ->
      let size = computeSize' a cnt 0 in
      let (rnd1, rnd2) = randomSplit rnd in
      let x = withTime (fun _ -> run g0 size rnd1) in
      aux n' (Stdlib.Int.succ cnt) (x :: acc) rnd2)
      n0
  in aux a.maxSuccess 0 [] newRandomSeed

(** val print_extracted_coq_string : char list -> unit **)

let print_extracted_coq_string = fun l -> print_string (
   let s = Bytes.create (List.length l) in
   let rec copy i = function
    | [] -> s
    | c :: l -> Bytes.set s i c; copy (i+1) l
   in Bytes.to_string (copy 0 l))

(** val genNatSized : int genSized **)

let genNatSized x =
  choose (Obj.magic producerGen) chooseNat (0, x)

(** val genZSized : Big_int_Z.big_int genSized **)

let genZSized x =
  let z0 = Z.of_nat x in
  choose (Obj.magic producerGen) chooseZ ((Z.opp z0), z0)

(** val genNSized : Big_int_Z.big_int genSized **)

let genNSized x =
  let n0 = N.of_nat x in
  choose (Obj.magic producerGen) chooseN (Big_int_Z.zero_big_int, n0)

type color =
| R
| B

(** val showColor : color show **)

let showColor = function
| R -> 'R'::[]
| B -> 'B'::[]

type tree =
| E
| T of color * tree * Big_int_Z.big_int * Big_int_Z.big_int * tree

(** val showTree : tree show **)

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

(** val blacken_ : tree -> tree **)

let blacken_ = function
| E -> E
| T (_, a, x, vx, b) -> T (B, a, x, vx, b)

(** val balance_ :
    color -> tree -> Big_int_Z.big_int -> Big_int_Z.big_int -> tree -> tree **)

let balance_ col tl key val0 tr =
  match col with
  | R -> T (col, tl, key, val0, tr)
  | B ->
    (match tl with
     | E ->
       (match tr with
        | E -> T (col, tl, key, val0, tr)
        | T (c0, b, y, vy, d) ->
          (match c0 with
           | R ->
             (match b with
              | E ->
                (match d with
                 | E -> T (col, tl, key, val0, tr)
                 | T (c1, c, z0, vz, d0) ->
                   (match c1 with
                    | R ->
                      T (R, (T (B, tl, key, val0, b)), y, vy, (T (B, c, z0,
                        vz, d0)))
                    | B -> T (col, tl, key, val0, tr)))
              | T (c1, b0, y0, vy0, c) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, tl, key, val0, b0)), y0, vy0, (T (B, c, y,
                     vy, d)))
                 | B ->
                   (match d with
                    | E -> T (col, tl, key, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key, val0, tr)))))
           | B -> T (col, tl, key, val0, tr)))
     | T (c0, a, x, vx, c) ->
       (match c0 with
        | R ->
          (match a with
           | E ->
             (match c with
              | E ->
                (match tr with
                 | E -> T (col, tl, key, val0, tr)
                 | T (c1, b, y, vy, d) ->
                   (match c1 with
                    | R ->
                      (match b with
                       | E ->
                         (match d with
                          | E -> T (col, tl, key, val0, tr)
                          | T (c2, c3, z0, vz, d0) ->
                            (match c2 with
                             | R ->
                               T (R, (T (B, tl, key, val0, b)), y, vy, (T (B,
                                 c3, z0, vz, d0)))
                             | B -> T (col, tl, key, val0, tr)))
                       | T (c2, b0, y0, vy0, c3) ->
                         (match c2 with
                          | R ->
                            T (R, (T (B, tl, key, val0, b0)), y0, vy0, (T (B,
                              c3, y, vy, d)))
                          | B ->
                            (match d with
                             | E -> T (col, tl, key, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key, val0, b)), y, vy, (T
                                    (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key, val0, tr)))))
                    | B -> T (col, tl, key, val0, tr)))
              | T (c1, b, y, vy, c2) ->
                (match c1 with
                 | R ->
                   T (R, (T (B, a, x, vx, b)), y, vy, (T (B, c2, key, val0,
                     tr)))
                 | B ->
                   (match tr with
                    | E -> T (col, tl, key, val0, tr)
                    | T (c3, b0, y0, vy0, d) ->
                      (match c3 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key, val0, tr)
                             | T (c4, c5, z0, vz, d0) ->
                               (match c4 with
                                | R ->
                                  T (R, (T (B, tl, key, val0, b0)), y0, vy0,
                                    (T (B, c5, z0, vz, d0)))
                                | B -> T (col, tl, key, val0, tr)))
                          | T (c4, b1, y1, vy1, c5) ->
                            (match c4 with
                             | R ->
                               T (R, (T (B, tl, key, val0, b1)), y1, vy1, (T
                                 (B, c5, y0, vy0, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key, val0, tr)
                                | T (c6, c7, z0, vz, d0) ->
                                  (match c6 with
                                   | R ->
                                     T (R, (T (B, tl, key, val0, b0)), y0,
                                       vy0, (T (B, c7, z0, vz, d0)))
                                   | B -> T (col, tl, key, val0, tr)))))
                       | B -> T (col, tl, key, val0, tr)))))
           | T (c1, a0, x0, vx0, b) ->
             (match c1 with
              | R ->
                T (R, (T (B, a0, x0, vx0, b)), x, vx, (T (B, c, key, val0,
                  tr)))
              | B ->
                (match c with
                 | E ->
                   (match tr with
                    | E -> T (col, tl, key, val0, tr)
                    | T (c2, b0, y, vy, d) ->
                      (match c2 with
                       | R ->
                         (match b0 with
                          | E ->
                            (match d with
                             | E -> T (col, tl, key, val0, tr)
                             | T (c3, c4, z0, vz, d0) ->
                               (match c3 with
                                | R ->
                                  T (R, (T (B, tl, key, val0, b0)), y, vy, (T
                                    (B, c4, z0, vz, d0)))
                                | B -> T (col, tl, key, val0, tr)))
                          | T (c3, b1, y0, vy0, c4) ->
                            (match c3 with
                             | R ->
                               T (R, (T (B, tl, key, val0, b1)), y0, vy0, (T
                                 (B, c4, y, vy, d)))
                             | B ->
                               (match d with
                                | E -> T (col, tl, key, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key, val0, b0)), y, vy,
                                       (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key, val0, tr)))))
                       | B -> T (col, tl, key, val0, tr)))
                 | T (c2, b0, y, vy, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, a, x, vx, b0)), y, vy, (T (B, c3, key,
                        val0, tr)))
                    | B ->
                      (match tr with
                       | E -> T (col, tl, key, val0, tr)
                       | T (c4, b1, y0, vy0, d) ->
                         (match c4 with
                          | R ->
                            (match b1 with
                             | E ->
                               (match d with
                                | E -> T (col, tl, key, val0, tr)
                                | T (c5, c6, z0, vz, d0) ->
                                  (match c5 with
                                   | R ->
                                     T (R, (T (B, tl, key, val0, b1)), y0,
                                       vy0, (T (B, c6, z0, vz, d0)))
                                   | B -> T (col, tl, key, val0, tr)))
                             | T (c5, b2, y1, vy1, c6) ->
                               (match c5 with
                                | R ->
                                  T (R, (T (B, tl, key, val0, b2)), y1, vy1,
                                    (T (B, c6, y0, vy0, d)))
                                | B ->
                                  (match d with
                                   | E -> T (col, tl, key, val0, tr)
                                   | T (c7, c8, z0, vz, d0) ->
                                     (match c7 with
                                      | R ->
                                        T (R, (T (B, tl, key, val0, b1)), y0,
                                          vy0, (T (B, c8, z0, vz, d0)))
                                      | B -> T (col, tl, key, val0, tr)))))
                          | B -> T (col, tl, key, val0, tr)))))))
        | B ->
          (match tr with
           | E -> T (col, tl, key, val0, tr)
           | T (c1, b, y, vy, d) ->
             (match c1 with
              | R ->
                (match b with
                 | E ->
                   (match d with
                    | E -> T (col, tl, key, val0, tr)
                    | T (c2, c3, z0, vz, d0) ->
                      (match c2 with
                       | R ->
                         T (R, (T (B, tl, key, val0, b)), y, vy, (T (B, c3,
                           z0, vz, d0)))
                       | B -> T (col, tl, key, val0, tr)))
                 | T (c2, b0, y0, vy0, c3) ->
                   (match c2 with
                    | R ->
                      T (R, (T (B, tl, key, val0, b0)), y0, vy0, (T (B, c3,
                        y, vy, d)))
                    | B ->
                      (match d with
                       | E -> T (col, tl, key, val0, tr)
                       | T (c4, c5, z0, vz, d0) ->
                         (match c4 with
                          | R ->
                            T (R, (T (B, tl, key, val0, b)), y, vy, (T (B,
                              c5, z0, vz, d0)))
                          | B -> T (col, tl, key, val0, tr)))))
              | B -> T (col, tl, key, val0, tr)))))

(** val insert_ : Big_int_Z.big_int -> Big_int_Z.big_int -> tree -> tree **)

let rec insert_ key val0 t =
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
  blacken_ (ins key val0 t)

(** val gen_tree : int -> tree g **)

let gen_tree s =
  bind (Obj.magic monadGen)
    (choose (Obj.magic producerGen) chooseNat ((Stdlib.Int.succ 0), s))
    (fun sz ->
    bind (Obj.magic monadGen) (Obj.magic gen_kvs sz) (fun kvs ->
      ret (Obj.magic monadGen)
        (fold_right (fun pat t -> let (k, v) = pat in insert_ k v t) E kvs)))

(** val bespoke : tree g **)

let bespoke =
  gen_tree (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))

(** val showTimedResult : 'a1 show -> 'a1 timedResult show **)

let showTimedResult h = function
| TResult (result0, time, _, _) ->
  append
    ('{'::(' '::('"'::('t'::('i'::('m'::('e'::('"'::(':'::(' '::('"'::[])))))))))))
    (append (showOCamlFloat time)
      (append
        ('"'::(','::(' '::('"'::('v'::('a'::('l'::('u'::('e'::('"'::(':'::(' '::('"'::[])))))))))))))
        (append (h result0) ('"'::(' '::('}'::[]))))))

(** val showTimedResultList :
    'a1 show -> 'a1 timedResult list -> char list **)

let showTimedResultList h results =
  append ('['::[])
    (append
      (concat (','::(' '::[])) (map (fun r -> showTimedResult h r) results))
      (']'::[]))

type ('a, 'b) args2 =
| Args2Mk of 'a * 'b

(** val showArgs2 : 'a1 show -> 'a2 show -> ('a1, 'a2) args2 show **)

let showArgs2 h h0 = function
| Args2Mk (a, b) ->
  append ('('::[]) (append (h a) (append (' '::[]) (append (h0 b) (')'::[]))))

type ('a, 'b, 'c) args3 =
| Args3Mk of 'a * 'b * 'c

(** val showArgs3 :
    'a1 show -> 'a2 show -> 'a3 show -> ('a1, 'a2, 'a3) args3 show **)

let showArgs3 h h0 h1 = function
| Args3Mk (a, b, c) ->
  append ('('::[])
    (append (h a)
      (append (' '::[])
        (append (h0 b) (append (' '::[]) (append (h1 c) (')'::[]))))))

type ('a, 'b, 'c, 'd) args4 =
| Args4Mk of 'a * 'b * 'c * 'd

(** val showArgs4 :
    'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> ('a1, 'a2, 'a3, 'a4)
    args4 show **)

let showArgs4 h h0 h1 h2 = function
| Args4Mk (a, b, c, d) ->
  append ('('::[])
    (append (h a)
      (append (' '::[])
        (append (h0 b)
          (append (' '::[])
            (append (h1 c) (append (' '::[]) (append (h2 d) (')'::[]))))))))

type ('a, 'b, 'c, 'd, 'e) args5 =
| Args5Mk of 'a * 'b * 'c * 'd * 'e

(** val showArgs5 :
    'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> 'a5 show -> ('a1, 'a2,
    'a3, 'a4, 'a5) args5 show **)

let showArgs5 h h0 h1 h2 h3 = function
| Args5Mk (a, b, c, d, e) ->
  append ('('::[])
    (append (h a)
      (append (' '::[])
        (append (h0 b)
          (append (' '::[])
            (append (h1 c)
              (append (' '::[])
                (append (h2 d) (append (' '::[]) (append (h3 e) (')'::[]))))))))))

(** val sample_InsertValid :
    (tree, Big_int_Z.big_int, Big_int_Z.big_int) args3 g **)

let sample_InsertValid =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNSized) (fun k ->
      bindGen (genOfGenSized genNSized) (fun v ->
        ret (Obj.magic monadGen) (Args3Mk (t, k, v)))))

(** val qctest_sample_InsertValid : int -> unit **)

let qctest_sample_InsertValid num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs3 showTree showN showN)
      (quickSample (showArgs3 showTree showN showN)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_InsertValid))

(** val sample_DeleteValid : (tree, int) args2 g **)

let sample_DeleteValid =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      ret (Obj.magic monadGen) (Args2Mk (t, k))))

(** val qctest_sample_DeleteValid : int -> unit **)

let qctest_sample_DeleteValid num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs2 showTree showNat)
      (quickSample (showArgs2 showTree showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_DeleteValid))

(** val sample_InsertPost : (tree, int, int, int) args4 g **)

let sample_InsertPost =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        bindGen (genOfGenSized genNatSized) (fun v ->
          ret (Obj.magic monadGen) (Args4Mk (t, k, k', v))))))

(** val qctest_sample_InsertPost : int -> unit **)

let qctest_sample_InsertPost num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs4 showTree showNat showNat showNat)
      (quickSample (showArgs4 showTree showNat showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_InsertPost))

(** val sample_DeletePost : (tree, int, int) args3 g **)

let sample_DeletePost =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        ret (Obj.magic monadGen) (Args3Mk (t, k, k')))))

(** val qctest_sample_DeletePost : int -> unit **)

let qctest_sample_DeletePost num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs3 showTree showNat showNat)
      (quickSample (showArgs3 showTree showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_DeletePost))

(** val sample_InsertModel : (tree, int, int) args3 g **)

let sample_InsertModel =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun v ->
        ret (Obj.magic monadGen) (Args3Mk (t, k, v)))))

(** val qctest_sample_InsertModel : int -> unit **)

let qctest_sample_InsertModel num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs3 showTree showNat showNat)
      (quickSample (showArgs3 showTree showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_InsertModel))

(** val sample_DeleteModel : (tree, int) args2 g **)

let sample_DeleteModel =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      ret (Obj.magic monadGen) (Args2Mk (t, k))))

(** val qctest_sample_DeleteModel : int -> unit **)

let qctest_sample_DeleteModel num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs2 showTree showNat)
      (quickSample (showArgs2 showTree showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_DeleteModel))

(** val sample_InsertInsert : (tree, int, int, int, int) args5 g **)

let sample_InsertInsert =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        bindGen (genOfGenSized genNatSized) (fun v ->
          bindGen (genOfGenSized genNatSized) (fun v' ->
            ret (Obj.magic monadGen) (Args5Mk (t, k, k', v, v')))))))

(** val qctest_sample_InsertInsert : int -> unit **)

let qctest_sample_InsertInsert num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs5 showTree showNat showNat showNat showNat)
      (quickSample (showArgs5 showTree showNat showNat showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_InsertInsert))

(** val sample_InsertDelete : (tree, int, int, int) args4 g **)

let sample_InsertDelete =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        bindGen (genOfGenSized genNatSized) (fun v ->
          ret (Obj.magic monadGen) (Args4Mk (t, k, k', v))))))

(** val qctest_sample_InsertDelete : int -> unit **)

let qctest_sample_InsertDelete num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs4 showTree showNat showNat showNat)
      (quickSample (showArgs4 showTree showNat showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_InsertDelete))

(** val sample_DeleteInsert : (tree, int, int, int) args4 g **)

let sample_DeleteInsert =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        bindGen (genOfGenSized genNatSized) (fun v' ->
          ret (Obj.magic monadGen) (Args4Mk (t, k, k', v'))))))

(** val qctest_sample_DeleteInsert : int -> unit **)

let qctest_sample_DeleteInsert num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs4 showTree showNat showNat showNat)
      (quickSample (showArgs4 showTree showNat showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_DeleteInsert))

(** val sample_DeleteDelete : (tree, int, int) args3 g **)

let sample_DeleteDelete =
  bindGen bespoke (fun t ->
    bindGen (genOfGenSized genNatSized) (fun k ->
      bindGen (genOfGenSized genNatSized) (fun k' ->
        ret (Obj.magic monadGen) (Args3Mk (t, k, k')))))

(** val qctest_sample_DeleteDelete : int -> unit **)

let qctest_sample_DeleteDelete num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs3 showTree showNat showNat)
      (quickSample (showArgs3 showTree showNat showNat)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_DeleteDelete))

type oCamlString = string

(** val test_map : (oCamlString * (int -> unit)) list **)

let test_map = [
    ("InsertValid", qctest_sample_InsertValid);
    ("DeleteValid", qctest_sample_DeleteValid);
    ("InsertPost", qctest_sample_InsertPost);
    ("DeletePost", qctest_sample_DeletePost);
    ("InsertModel", qctest_sample_InsertModel);
    ("DeleteModel", qctest_sample_DeleteModel);
    ("InsertInsert", qctest_sample_InsertInsert);
    ("InsertDelete", qctest_sample_InsertDelete);
    ("DeleteInsert", qctest_sample_DeleteInsert);
    ("DeleteDelete", qctest_sample_DeleteDelete);
]

(** val qctest_map : oCamlString -> int -> unit **)

let qctest_map = 
fun property num_tests ->
  let test = List.assoc property test_map in
  test num_tests


let () =
  let args = Sys.argv in
  if Array.length args <> 3 then
    Printf.eprintf "Usage: %s <property> <#tests>\n" args.(0)
  else
    let property = args.(1) in
    let num_tests = int_of_string args.(2) in
    if not (List.mem_assoc property test_map) then
      Printf.eprintf "Unknown test name: %s\n" property
    else
      qctest_map property num_tests


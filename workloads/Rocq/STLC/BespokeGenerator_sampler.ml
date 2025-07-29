
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Stdlib.Int.succ (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

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
 end

(** val add : int -> int -> int **)

let rec add = (+)

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Stdlib.max 0 (n-m)



type reflect =
| ReflectT
| ReflectF

module Nat =
 struct
  (** val sub : int -> int -> int **)

  let rec sub = (-)

  (** val eqb : int -> int -> bool **)

  let rec eqb = (=)

  (** val leb : int -> int -> bool **)

  let rec leb = (<=)

  (** val to_little_uint : int -> uint -> uint **)

  let rec to_little_uint n acc =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> acc)
      (fun n0 -> to_little_uint n0 (Little.succ acc))
      n

  (** val to_uint : int -> uint **)

  let to_uint n =
    rev (to_little_uint n (D0 Nil))

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

(** val nth : int -> 'a1 list -> 'a1 -> 'a1 **)

let rec nth n l default =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match l with
              | [] -> default
              | x :: _ -> x)
    (fun m -> match l with
              | [] -> default
              | _ :: t -> nth m t default)
    n

(** val rev0 : 'a1 list -> 'a1 list **)

let rec rev0 = function
| [] -> []
| x :: l' -> app (rev0 l') (x :: [])

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list **)

let rec combine l l' =
  match l with
  | [] -> []
  | x :: tl ->
    (match l' with
     | [] -> []
     | y :: tl' -> (x, y) :: (combine tl tl'))

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

(** val showNat : int show **)

let showNat =
  show_nat

type 'm monad = { ret : (__ -> __ -> 'm);
                  bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

(** val ret : 'a1 monad -> 'a2 -> 'a1 **)

let ret monad0 x =
  Obj.magic monad0.ret __ x

(** val bind : 'a1 monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad0 x x0 =
  Obj.magic monad0.bind __ __ x x0

type decidable = bool

(** val iffP : bool -> reflect -> reflect **)

let iffP _ pb =
  let _evar_0_ = fun _ _ _ -> ReflectT in
  let _evar_0_0 = fun _ _ _ -> ReflectF in
  (match pb with
   | ReflectT -> _evar_0_ __ __ __
   | ReflectF -> _evar_0_0 __ __ __)

(** val idP : bool -> reflect **)

let idP = function
| true -> ReflectT
| false -> ReflectF

type 't pred = 't -> bool

type 't rel = 't -> 't pred

module Equality =
 struct
  type 't axiom = 't -> 't -> reflect

  type 't mixin_of = { op : 't rel; mixin_of__1 : 't axiom }

  (** val op : 'a1 mixin_of -> 'a1 rel **)

  let op m =
    m.op

  type coq_type =
    __ mixin_of
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  (** val coq_class : coq_type -> sort mixin_of **)

  let coq_class cT =
    cT
 end

(** val eq_op : Equality.coq_type -> Equality.sort rel **)

let eq_op t =
  (Equality.coq_class t).Equality.op

(** val eqn : int -> int -> bool **)

let rec eqn = (==)

(** val eqnP : int Equality.axiom **)

let eqnP n m =
  iffP (eqn n m) (idP (eqn n m))

(** val nat_eqMixin : int Equality.mixin_of **)

let nat_eqMixin =
  { Equality.op = eqn; Equality.mixin_of__1 = eqnP }

(** val nat_eqType : Equality.coq_type **)

let nat_eqType =
  Obj.magic nat_eqMixin

(** val addn_rec : int -> int -> int **)

let addn_rec =
  add

(** val addn : int -> int -> int **)

let addn =
  addn_rec

(** val subn_rec : int -> int -> int **)

let subn_rec =
  sub

(** val subn : int -> int -> int **)

let subn =
  subn_rec

(** val leq : int -> int -> bool **)

let leq m n =
  eq_op nat_eqType (Obj.magic subn m n) (Obj.magic 0)

(** val nth0 : 'a1 -> 'a1 list -> int -> 'a1 **)

let rec nth0 x0 s n =
  match s with
  | [] -> x0
  | x :: s' ->
    ((fun fO fS n -> if n=0 then fO () else fS (n-1))
       (fun _ -> x)
       (fun n' -> nth0 x0 s' n')
       n)

(** val foldl : ('a2 -> 'a1 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec foldl f z = function
| [] -> z
| x :: s' -> foldl f (f z x) s'

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

(** val oneOf_ : 'a1 producer -> 'a1 -> 'a1 list -> 'a1 **)

let oneOf_ pG def gs =
  bind pG.super
    (choose pG chooseNat (0, (subn (length gs) (Stdlib.Int.succ 0))))
    (nth0 def gs)

(** val elems_ : 'a1 producer -> 'a2 -> 'a2 list -> 'a1 **)

let elems_ pG def l =
  let n = length l in
  bind pG.super (choose pG chooseNat (0, (subn n (Stdlib.Int.succ 0))))
    (fun n' -> ret pG.super (nth n' l def))

(** val bindOpt : 'a1 producer -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bindOpt pG g0 f =
  bind pG.super g0 (fun ma ->
    match ma with
    | Some a -> f a
    | None -> ret pG.super None)

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

let bindGen g0 k n r =
  let (r1, r2) = randomSplit r in run (k (run g0 n r1)) n r2

(** val monadGen : __ g monad **)

let monadGen =
  { ret = (fun _ -> returnGen); bind = (fun _ _ -> bindGen) }

(** val createRange : int -> int list -> int list **)

let rec createRange n acc =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> rev0 (0 :: acc))
    (fun n' -> createRange n' (n :: acc))
    n

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
  map (fun p -> let (r, n) = p in g0 n r) l

(** val sizedGen : (int -> 'a1 g) -> 'a1 g **)

let sizedGen f n r =
  run (f n) n r

(** val resizeGen : int -> 'a1 g -> 'a1 g **)

let resizeGen n g0 _ =
  g0 n

(** val chooseGen : 'a1 choosableFromInterval -> ('a1 * 'a1) -> 'a1 g **)

let chooseGen h range _ r =
  fst (h.randomR range r)

(** val producerGen : __ g producer **)

let producerGen =
  { super = monadGen; sample = (fun _ -> sampleGen); sized = (fun _ ->
    sizedGen); resize = (fun _ -> resizeGen); choose = (fun _ _ ->
    chooseGen); bindPf = (fun _ _ g0 k n r ->
    let (r1, r2) = randomSplit r in run (k (run g0 n r1) __) n r2) }

(** val thunkGen : (unit -> 'a1 g) -> 'a1 g **)

let thunkGen f n r =
  run (f ()) n r

(** val pick : 'a1 g -> (int * 'a1 g) list -> int -> int * 'a1 g **)

let rec pick def xs n =
  match xs with
  | [] -> (0, def)
  | p :: xs0 ->
    let (k, x) = p in
    if leq (Stdlib.Int.succ n) k then (k, x) else pick def xs0 (subn n k)

(** val pickDrop :
    (int * 'a1 option g) list -> int -> (int * 'a1 option g) * (int * 'a1
    option g) list **)

let rec pickDrop xs n =
  match xs with
  | [] -> ((0, (ret (Obj.magic monadGen) None)), [])
  | p :: xs0 ->
    let (k, x) = p in
    if leq (Stdlib.Int.succ n) k
    then ((k, x), xs0)
    else let (p0, xs') = pickDrop xs0 (subn n k) in (p0, ((k, x) :: xs'))

(** val sum_fst : (int * 'a1) list -> int **)

let sum_fst gs =
  foldl (fun t p -> addn t (fst p)) 0 gs

(** val freq_ : 'a1 g -> (int * 'a1 g) list -> 'a1 g **)

let freq_ def gs =
  let tot = sum_fst gs in
  bindGen
    (choose (Obj.magic producerGen) chooseNat (0,
      (subn tot (Stdlib.Int.succ 0)))) (fun n -> snd (pick def gs n))

(** val backtrackFuel :
    int -> int -> (int * 'a1 option g) list -> 'a1 option g **)

let rec backtrackFuel fuel tot gs =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> ret (Obj.magic monadGen) None)
    (fun fuel' ->
    bindGen
      (choose (Obj.magic producerGen) chooseNat (0,
        (subn tot (Stdlib.Int.succ 0)))) (fun n ->
      let (p, gs') = pickDrop gs n in
      let (k, g0) = p in
      bindGen g0 (fun ma ->
        match ma with
        | Some a -> ret (Obj.magic monadGen) (Some a)
        | None -> backtrackFuel fuel' (subn tot k) gs')))
    fuel

(** val backtrack : (int * 'a1 option g) list -> 'a1 option g **)

let backtrack gs =
  backtrackFuel (length gs) (sum_fst gs) gs

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

let roundTo n m =
  mul (Nat.div n m) m

(** val computeSize'' : int -> int -> int -> int -> int **)

let computeSize'' maxSize_ maxSuccess_ n d =
  if (||)
       ((||) (Nat.leb (add (roundTo n maxSize_) maxSize_) maxSuccess_)
         (Nat.leb maxSuccess_ n))
       (Nat.eqb (Nat.modulo maxSuccess_ maxSize_) 0)
  then Stdlib.min
         (add (Nat.modulo n maxSize_)
           (Nat.div d (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ 0)))))))))))) maxSize_
  else Stdlib.min
         (Nat.div (mul (Nat.modulo n maxSize_) maxSize_)
           (add (Nat.modulo maxSuccess_ maxSize_)
             (Nat.div d (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
               (Stdlib.Int.succ 0))))))))))))) maxSize_

(** val computeSize' : args -> int -> int -> int **)

let computeSize' a n d =
  computeSize'' a.maxSize a.maxSuccess n d

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
  let rec aux n cnt acc rnd =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> rev0 acc)
      (fun n' ->
      let size = computeSize' a cnt 0 in
      let (rnd1, rnd2) = randomSplit rnd in
      let x = withTime (fun _ -> run g0 size rnd1) in
      aux n' (Stdlib.Int.succ cnt) (x :: acc) rnd2)
      n
  in aux a.maxSuccess 0 [] newRandomSeed

(** val print_extracted_coq_string : char list -> unit **)

let print_extracted_coq_string = fun l -> print_string (
   let s = Bytes.create (List.length l) in
   let rec copy i = function
    | [] -> s
    | c :: l -> Bytes.set s i c; copy (i+1) l
   in Bytes.to_string (copy 0 l))

type dec = decidable
  (* singleton inductive, whose constructor was Build_Dec *)

(** val genBoolSized : bool genSized **)

let genBoolSized _ =
  elems_ (Obj.magic producerGen) true (true :: (false :: []))

type typ =
| TBool
| TFun of typ * typ

(** val showTyp : typ show **)

let rec showTyp = function
| TBool -> '('::('T'::('B'::('o'::('o'::('l'::(')'::[]))))))
| TFun (t1, t2) ->
  append ('('::('T'::('F'::('u'::('n'::(' '::[]))))))
    (append (showTyp t1) (append (' '::[]) (append (showTyp t2) (')'::[]))))

type expr =
| Var of int
| Bool of bool
| Abs of typ * expr
| App of expr * expr

(** val sexp : bool -> char list **)

let sexp = function
| true -> '#'::('t'::[])
| false -> '#'::('f'::[])

(** val showExpr : expr show **)

let rec showExpr = function
| Var n ->
  append ('('::('V'::('a'::('r'::(' '::[]))))) (append (showNat n) (')'::[]))
| Bool b ->
  append ('('::('B'::('o'::('o'::('l'::(' '::[]))))))
    (append (sexp b) (')'::[]))
| Abs (t, e') ->
  append ('('::('A'::('b'::('s'::(' '::[])))))
    (append (showTyp t) (append (' '::[]) (append (showExpr e') (')'::[]))))
| App (e1, e2) ->
  append ('('::('A'::('p'::('p'::(' '::[])))))
    (append (showExpr e1) (append (' '::[]) (append (showExpr e2) (')'::[]))))

type ctx = typ list

(** val dec_type : typ -> typ -> dec **)

let rec dec_type t x =
  match t with
  | TBool -> (match x with
              | TBool -> true
              | TFun (_, _) -> false)
  | TFun (t0, t1) ->
    (match x with
     | TBool -> false
     | TFun (t2, t3) -> if dec_type t0 t2 then dec_type t1 t3 else false)

(** val genSizedTyp0 : typ genSized **)

let rec genSizedTyp0 size =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> returnGen TBool)
    (fun size' ->
    freq_ (returnGen TBool) (((Stdlib.Int.succ 0),
      (thunkGen (fun _ -> returnGen TBool))) :: (((Stdlib.Int.succ size'),
      (thunkGen (fun _ ->
        bindGen (genSizedTyp0 size') (fun p0 ->
          bindGen (genSizedTyp0 size') (fun p1 -> returnGen (TFun (p0, p1))))))) :: [])))
    size

(** val genVar' : ctx -> typ -> int -> int list -> int list **)

let rec genVar' ctx0 t p r =
  match ctx0 with
  | [] -> r
  | t' :: ctx' ->
    if dec_type t t'
    then genVar' ctx' t (add p (Stdlib.Int.succ 0)) (p :: r)
    else genVar' ctx' t (add p (Stdlib.Int.succ 0)) r

(** val genZero : typ list -> typ -> expr option g **)

let rec genZero env = function
| TBool ->
  bindGen (genOfGenSized genBoolSized) (fun b -> returnGen (Some (Bool b)))
| TFun (t1, t2) ->
  bindOpt (Obj.magic producerGen) (genZero (t1 :: env) t2) (fun e ->
    returnGen (Some (Abs (t1, e))))

(** val genExpr : ctx -> typ -> int -> expr option g **)

let rec genExpr env tau sz =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    backtrack (((Stdlib.Int.succ 0),
      (oneOf_ (Obj.magic producerGen) (ret (Obj.magic monadGen) None)
        (map (fun x -> returnGen (Some (Var x))) (genVar' env tau 0 [])))) :: (((Stdlib.Int.succ
      0), (genZero env tau)) :: [])))
    (fun sz' ->
    backtrack (((Stdlib.Int.succ 0),
      (oneOf_ (Obj.magic producerGen) (ret (Obj.magic monadGen) None)
        (map (fun x -> returnGen (Some (Var x))) (genVar' env tau 0 [])))) :: (((Stdlib.Int.succ
      0),
      (bindGen (genOfGenSized genSizedTyp0) (fun t1 ->
        bindOpt (Obj.magic producerGen) (genExpr env (TFun (t1, tau)) sz')
          (fun e1 ->
          bindOpt (Obj.magic producerGen) (genExpr env t1 sz') (fun e2 ->
            returnGen (Some (App (e1, e2)))))))) :: (((Stdlib.Int.succ 0),
      (match tau with
       | TBool ->
         bindGen (genOfGenSized genBoolSized) (fun b ->
           returnGen (Some (Bool b)))
       | TFun (t1, t2) ->
         bindOpt (Obj.magic producerGen) (genExpr (t1 :: env) t2 sz')
           (fun e -> returnGen (Some (Abs (t1, e)))))) :: []))))
    sz

(** val gSized : expr option g **)

let gSized =
  bind (Obj.magic monadGen) (genOfGenSized (Obj.magic genSizedTyp0))
    (fun typ0 ->
    genExpr [] typ0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ 0))))))

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

type 'a args1 = 'a
  (* singleton inductive, whose constructor was Args1Mk *)

(** val showArgs1 : 'a1 show -> 'a1 args1 show **)

let showArgs1 h =
  h

(** val showExprOpt : expr option show **)

let showExprOpt = function
| Some e' -> showExpr e'
| None -> []

(** val sample_SinglePreserve : expr option args1 g **)

let sample_SinglePreserve =
  bindGen gSized (fun e -> ret (Obj.magic monadGen) e)

(** val qctest_sample_SinglePreserve : int -> unit **)

let qctest_sample_SinglePreserve num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs1 showExprOpt)
      (quickSample (showArgs1 showExprOpt)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_SinglePreserve))

(** val sample_MultiPreserve : expr option args1 g **)

let sample_MultiPreserve =
  bindGen gSized (fun e -> ret (Obj.magic monadGen) e)

(** val qctest_sample_MultiPreserve : int -> unit **)

let qctest_sample_MultiPreserve num_tests =
  print_extracted_coq_string
    (showTimedResultList (showArgs1 showExprOpt)
      (quickSample (showArgs1 showExprOpt)
        (updMaxDiscard (updMaxSuccess (updAnalysis stdArgs true) num_tests)
          num_tests) sample_MultiPreserve))

type oCamlString = string

(** val test_map : (oCamlString * (int -> unit)) list **)

let test_map = [
    ("SinglePreserve", qctest_sample_SinglePreserve);
    ("MultiPreserve", qctest_sample_MultiPreserve);
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


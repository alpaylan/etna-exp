
type __ = Obj.t

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val app : 'a1 list -> 'a1 list -> 'a1 list

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

val revapp : uint -> uint -> uint

val rev : uint -> uint

module Little :
 sig
  val succ : uint -> uint

  val double : uint -> uint

  val succ_double : uint -> uint
 end

val add : int -> int -> int

val mul : int -> int -> int

val sub : int -> int -> int



type decidable =
  bool
  (* singleton inductive, whose constructor was Build_Decidable *)

type reflect =
| ReflectT
| ReflectF

module Nat :
 sig
  val sub : int -> int -> int

  val eqb : int -> int -> bool

  val leb : int -> int -> bool

  val to_little_uint : int -> uint -> uint

  val to_uint : int -> uint

  val divmod : int -> int -> int -> int -> int * int

  val div : int -> int -> int

  val modulo : int -> int -> int

  val coq_Decidable_le_nat : int -> int -> decidable
 end

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of Big_int_Z.big_int
  | IsNeg
 end

module Coq_Pos :
 sig
  val succ : Big_int_Z.big_int -> Big_int_Z.big_int

  val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int

  type mask = Pos.mask =
  | IsNul
  | IsPos of Big_int_Z.big_int
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : Big_int_Z.big_int -> mask

  val sub_mask : Big_int_Z.big_int -> Big_int_Z.big_int -> mask

  val sub_mask_carry : Big_int_Z.big_int -> Big_int_Z.big_int -> mask

  val iter_op : ('a1 -> 'a1 -> 'a1) -> Big_int_Z.big_int -> 'a1 -> 'a1

  val to_nat : Big_int_Z.big_int -> int

  val of_succ_nat : int -> Big_int_Z.big_int

  val to_little_uint : Big_int_Z.big_int -> uint

  val to_uint : Big_int_Z.big_int -> uint
 end

module N :
 sig
  val succ : Big_int_Z.big_int -> Big_int_Z.big_int

  val sub : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val to_nat : Big_int_Z.big_int -> int

  val of_nat : int -> Big_int_Z.big_int
 end

val rev0 : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list

module Z :
 sig
  val of_N : Big_int_Z.big_int -> Big_int_Z.big_int

  val to_int :
    Big_int_Z.big_int ->
    ((Obj.t -> Obj.t) -> (Obj.t -> Obj.t) -> Obj.t) (* Decimal.int *)
 end

val append : char list -> char list -> char list

val concat : char list -> char list list -> char list

type 'a show =
  'a -> char list
  (* singleton inductive, whose constructor was Build_Show *)

val show_nat : int -> char list

val show_N : Big_int_Z.big_int -> char list

val showNat : int show

val showN : Big_int_Z.big_int show

type 'm monad = { ret : (__ -> __ -> 'm);
                  bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

val ret : 'a1 monad -> 'a2 -> 'a1

val bind : 'a1 monad -> 'a1 -> ('a2 -> 'a1) -> 'a1

type decidable0 = bool

val iffP : bool -> reflect -> reflect

val idP : bool -> reflect

type 't pred = 't -> bool

type 't rel = 't -> 't pred

module Equality :
 sig
  type 't axiom = 't -> 't -> reflect

  type 't mixin_of = { op : 't rel; mixin_of__1 : 't axiom }

  val op : 'a1 mixin_of -> 'a1 rel

  type coq_type =
    __ mixin_of
    (* singleton inductive, whose constructor was Pack *)

  type sort = __

  val coq_class : coq_type -> sort mixin_of
 end

val eq_op : Equality.coq_type -> Equality.sort rel

val eqn : int -> int -> bool

val eqnP : int Equality.axiom

val nat_eqMixin : int Equality.mixin_of

val nat_eqType : Equality.coq_type

val addn_rec : int -> int -> int

val addn : int -> int -> int

val subn_rec : int -> int -> int

val subn : int -> int -> int

val leq : int -> int -> bool

val foldl : ('a2 -> 'a1 -> 'a2) -> 'a2 -> 'a1 list -> 'a2

type 'a lazyList =
| Lnil
| Lcons of 'a * (unit -> 'a lazyList)

val lazy_seq : ('a1 -> 'a1) -> 'a1 -> int -> 'a1 lazyList

type randomSeed = Random.State.t

val newRandomSeed : randomSeed

val randomSplit : randomSeed -> randomSeed * randomSeed

val randomRNat : (int * int) -> randomSeed -> int * randomSeed

val randomRN :
  (Big_int_Z.big_int * Big_int_Z.big_int) -> randomSeed ->
  Big_int_Z.big_int * randomSeed

type 'a choosableFromInterval = { randomR : (('a * 'a) -> randomSeed ->
                                            'a * randomSeed);
                                  enumR : (('a * 'a) -> 'a lazyList) }

val enumRNat : (int * int) -> int lazyList

val chooseNat : int choosableFromInterval

val enumRN :
  (Big_int_Z.big_int * Big_int_Z.big_int) -> Big_int_Z.big_int lazyList

val chooseN : Big_int_Z.big_int choosableFromInterval

type 'g producer = { super : 'g monad; sample : (__ -> 'g -> __ list);
                     sized : (__ -> (int -> 'g) -> 'g);
                     resize : (__ -> int -> 'g -> 'g);
                     choose : (__ -> __ -> __ choosableFromInterval ->
                              (__ * __) -> 'g);
                     bindPf : (__ -> __ -> 'g -> (__ -> __ -> 'g) -> 'g) }

val sized : 'a1 producer -> (int -> 'a1) -> 'a1

val choose : 'a1 producer -> 'a2 choosableFromInterval -> ('a2 * 'a2) -> 'a1

type 'a genType =
  int -> randomSeed -> 'a
  (* singleton inductive, whose constructor was MkGen *)

type 'a g = 'a genType

val run : 'a1 g -> int -> randomSeed -> 'a1

val returnGen : 'a1 -> 'a1 g

val bindGen : 'a1 g -> ('a1 -> 'a2 g) -> 'a2 g

val monadGen : __ g monad

val createRange : int -> int list -> int list

val rnds : randomSeed -> int -> randomSeed list

val sampleGen : 'a1 g -> 'a1 list

val sizedGen : (int -> 'a1 g) -> 'a1 g

val resizeGen : int -> 'a1 g -> 'a1 g

val chooseGen : 'a1 choosableFromInterval -> ('a1 * 'a1) -> 'a1 g

val producerGen : __ g producer

val pick : 'a1 g -> (int * 'a1 g) list -> int -> int * 'a1 g

val sum_fst : (int * 'a1) list -> int

val freq_ : 'a1 g -> (int * 'a1 g) list -> 'a1 g

type 'a genSized =
  int -> 'a g
  (* singleton inductive, whose constructor was Build_GenSized *)

type 'a gen = 'a g
  (* singleton inductive, whose constructor was Build_Gen *)

val genOfGenSized : 'a1 genSized -> 'a1 gen

type args = { replay : (randomSeed * int) option; maxSuccess : int;
              maxDiscard : int; maxShrinks : int; maxSize : int;
              chatty : bool; analysis : bool }

val updMaxSuccess : args -> int -> args

val updMaxDiscard : args -> int -> args

val updAnalysis : args -> bool -> args

val defNumTests : int

val defNumDiscards : int

val defNumShrinks : int

val defSize : int

val doAnalysis : bool

val stdArgs : args

val roundTo : int -> int -> int

val computeSize'' : int -> int -> int -> int -> int

val computeSize' : args -> int -> int -> int

type oCamlFloat = float

val showOCamlFloat' : oCamlFloat -> char list

val showOCamlFloat : oCamlFloat show

type 'a timedResult =
| TResult of 'a * oCamlFloat * oCamlFloat * oCamlFloat

val withTime : (unit -> 'a1) -> 'a1 timedResult

val quickSample : 'a1 show -> args -> 'a1 g -> 'a1 timedResult list

val print_extracted_coq_string : char list -> unit

type dec =
  decidable0
  (* singleton inductive, whose constructor was Build_Dec *)

val dec_class_dec : decidable -> dec

val genNatSized : int genSized

val genNSized : Big_int_Z.big_int genSized

type tree =
| E
| T of tree * int * int * tree

val showTree : tree show

val gen_bst : int -> int -> int -> tree g

val bespoke : tree g

val showTimedResult : 'a1 show -> 'a1 timedResult show

val showTimedResultList : 'a1 show -> 'a1 timedResult list -> char list

type 'a args1 = 'a
  (* singleton inductive, whose constructor was Args1Mk *)

val showArgs1 : 'a1 show -> 'a1 args1 show

type ('a, 'b) args2 =
| Args2Mk of 'a * 'b

val showArgs2 : 'a1 show -> 'a2 show -> ('a1, 'a2) args2 show

type ('a, 'b, 'c) args3 =
| Args3Mk of 'a * 'b * 'c

val showArgs3 : 'a1 show -> 'a2 show -> 'a3 show -> ('a1, 'a2, 'a3) args3 show

type ('a, 'b, 'c, 'd) args4 =
| Args4Mk of 'a * 'b * 'c * 'd

val showArgs4 :
  'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> ('a1, 'a2, 'a3, 'a4) args4
  show

type ('a, 'b, 'c, 'd, 'e) args5 =
| Args5Mk of 'a * 'b * 'c * 'd * 'e

val showArgs5 :
  'a1 show -> 'a2 show -> 'a3 show -> 'a4 show -> 'a5 show -> ('a1, 'a2, 'a3,
  'a4, 'a5) args5 show

val sample_InsertValid : (tree, Big_int_Z.big_int, Big_int_Z.big_int) args3 g

val qctest_sample_InsertValid : int -> unit

val sample_DeleteValid : (tree, int) args2 g

val qctest_sample_DeleteValid : int -> unit

val sample_UnionValid : (tree, tree) args2 g

val qctest_sample_UnionValid : int -> unit

val sample_InsertPost : (tree, int, int, int) args4 g

val qctest_sample_InsertPost : int -> unit

val sample_DeletePost : (tree, int, int) args3 g

val qctest_sample_DeletePost : int -> unit

val sample_UnionPost : (tree, tree, int) args3 g

val qctest_sample_UnionPost : int -> unit

val sample_InsertModel : (tree, int, int) args3 g

val qctest_sample_InsertModel : int -> unit

val sample_DeleteModel : (tree, int) args2 g

val qctest_sample_DeleteModel : int -> unit

val sample_UnionModel : (tree, tree) args2 g

val qctest_sample_UnionModel : int -> unit

val sample_InsertInsert : (tree, int, int, int, int) args5 g

val qctest_sample_InsertInsert : int -> unit

val sample_InsertDelete : (tree, int, int, int) args4 g

val qctest_sample_InsertDelete : int -> unit

val sample_InsertUnion : (tree, tree, int, int) args4 g

val qctest_sample_InsertUnion : int -> unit

val sample_DeleteInsert : (tree, int, int, int) args4 g

val qctest_sample_DeleteInsert : int -> unit

val sample_DeleteDelete : (tree, int, int) args3 g

val qctest_sample_DeleteDelete : int -> unit

val sample_DeleteUnion : (tree, tree, int) args3 g

val qctest_sample_DeleteUnion : int -> unit

val sample_UnionDeleteInsert : (tree, tree, int, int) args4 g

val qctest_sample_UnionDeleteInsert : int -> unit

val sample_UnionUnionIdem : tree args1 g

val qctest_sample_UnionUnionIdem : int -> unit

val sample_UnionUnionAssoc : (tree, tree, tree) args3 g

val qctest_sample_UnionUnionAssoc : int -> unit

type oCamlString = string

val test_map : (oCamlString * (int -> unit)) list

val qctest_map : oCamlString -> int -> unit

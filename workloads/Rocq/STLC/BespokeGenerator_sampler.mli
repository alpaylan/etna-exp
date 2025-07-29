
type __ = Obj.t

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> int

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
 end

val add : int -> int -> int

val mul : int -> int -> int

val sub : int -> int -> int



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
 end

val nth : int -> 'a1 list -> 'a1 -> 'a1

val rev0 : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list

val append : char list -> char list -> char list

val concat : char list -> char list list -> char list

type 'a show =
  'a -> char list
  (* singleton inductive, whose constructor was Build_Show *)

val show_nat : int -> char list

val showNat : int show

type 'm monad = { ret : (__ -> __ -> 'm);
                  bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

val ret : 'a1 monad -> 'a2 -> 'a1

val bind : 'a1 monad -> 'a1 -> ('a2 -> 'a1) -> 'a1

type decidable = bool

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

val nth0 : 'a1 -> 'a1 list -> int -> 'a1

val foldl : ('a2 -> 'a1 -> 'a2) -> 'a2 -> 'a1 list -> 'a2

type 'a lazyList =
| Lnil
| Lcons of 'a * (unit -> 'a lazyList)

val lazy_seq : ('a1 -> 'a1) -> 'a1 -> int -> 'a1 lazyList

type randomSeed = Random.State.t

val newRandomSeed : randomSeed

val randomSplit : randomSeed -> randomSeed * randomSeed

val randomRNat : (int * int) -> randomSeed -> int * randomSeed

type 'a choosableFromInterval = { randomR : (('a * 'a) -> randomSeed ->
                                            'a * randomSeed);
                                  enumR : (('a * 'a) -> 'a lazyList) }

val enumRNat : (int * int) -> int lazyList

val chooseNat : int choosableFromInterval

type 'g producer = { super : 'g monad; sample : (__ -> 'g -> __ list);
                     sized : (__ -> (int -> 'g) -> 'g);
                     resize : (__ -> int -> 'g -> 'g);
                     choose : (__ -> __ -> __ choosableFromInterval ->
                              (__ * __) -> 'g);
                     bindPf : (__ -> __ -> 'g -> (__ -> __ -> 'g) -> 'g) }

val sized : 'a1 producer -> (int -> 'a1) -> 'a1

val choose : 'a1 producer -> 'a2 choosableFromInterval -> ('a2 * 'a2) -> 'a1

val oneOf_ : 'a1 producer -> 'a1 -> 'a1 list -> 'a1

val elems_ : 'a1 producer -> 'a2 -> 'a2 list -> 'a1

val bindOpt : 'a1 producer -> 'a1 -> ('a2 -> 'a1) -> 'a1

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

val thunkGen : (unit -> 'a1 g) -> 'a1 g

val pick : 'a1 g -> (int * 'a1 g) list -> int -> int * 'a1 g

val pickDrop :
  (int * 'a1 option g) list -> int -> (int * 'a1 option g) * (int * 'a1
  option g) list

val sum_fst : (int * 'a1) list -> int

val freq_ : 'a1 g -> (int * 'a1 g) list -> 'a1 g

val backtrackFuel : int -> int -> (int * 'a1 option g) list -> 'a1 option g

val backtrack : (int * 'a1 option g) list -> 'a1 option g

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

type dec = decidable
  (* singleton inductive, whose constructor was Build_Dec *)

val genBoolSized : bool genSized

type typ =
| TBool
| TFun of typ * typ

val showTyp : typ show

type expr =
| Var of int
| Bool of bool
| Abs of typ * expr
| App of expr * expr

val sexp : bool -> char list

val showExpr : expr show

type ctx = typ list

val dec_type : typ -> typ -> dec

val genSizedTyp0 : typ genSized

val genVar' : ctx -> typ -> int -> int list -> int list

val genZero : typ list -> typ -> expr option g

val genExpr : ctx -> typ -> int -> expr option g

val gSized : expr option g

val showTimedResult : 'a1 show -> 'a1 timedResult show

val showTimedResultList : 'a1 show -> 'a1 timedResult list -> char list

type 'a args1 = 'a
  (* singleton inductive, whose constructor was Args1Mk *)

val showArgs1 : 'a1 show -> 'a1 args1 show

val showExprOpt : expr option show

val sample_SinglePreserve : expr option args1 g

val qctest_sample_SinglePreserve : int -> unit

val sample_MultiPreserve : expr option args1 g

val qctest_sample_MultiPreserve : int -> unit

type oCamlString = string

val test_map : (oCamlString * (int -> unit)) list

val qctest_map : oCamlString -> int -> unit

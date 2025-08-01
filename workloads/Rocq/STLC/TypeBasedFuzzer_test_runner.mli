
type __ = Obj.t

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> int

val app : 'a1 list -> 'a1 list -> 'a1 list

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

val internal_eq_rew_r_dep : 'a1 -> 'a1 -> 'a2 -> 'a2

module type EqLtLe =
 sig
  type t
 end

module MakeOrderTac :
 functor (O:EqLtLe) ->
 functor (P:sig
 end) ->
 sig
 end

module Nat :
 sig
  val sub : int -> int -> int

  val eqb : int -> int -> bool

  val leb : int -> int -> bool

  val ltb : int -> int -> bool

  val to_little_uint : int -> uint -> uint

  val to_uint : int -> uint

  val divmod : int -> int -> int -> int -> int * int

  val div : int -> int -> int

  val modulo : int -> int -> int
 end

module Pos :
 sig
  val succ : Big_int_Z.big_int -> Big_int_Z.big_int

  val add : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val add_carry : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int

  val mul : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val compare_cont :
    comparison -> Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val iter_op : ('a1 -> 'a1 -> 'a1) -> Big_int_Z.big_int -> 'a1 -> 'a1

  val to_nat : Big_int_Z.big_int -> int

  val of_succ_nat : int -> Big_int_Z.big_int

  val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool
 end

module N :
 sig
  val add : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val mul : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison
 end

val n_of_digits : bool list -> Big_int_Z.big_int

val n_of_ascii : char -> Big_int_Z.big_int

val nth : int -> 'a1 list -> 'a1 -> 'a1

val nth_error : 'a1 list -> int -> 'a1 option

val rev0 : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

val combine : 'a1 list -> 'a2 list -> ('a1 * 'a2) list

module Z :
 sig
  val double : Big_int_Z.big_int -> Big_int_Z.big_int

  val succ_double : Big_int_Z.big_int -> Big_int_Z.big_int

  val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int

  val pos_sub : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val add : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val opp : Big_int_Z.big_int -> Big_int_Z.big_int

  val sub : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val mul : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val max : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val to_nat : Big_int_Z.big_int -> int

  val of_nat : int -> Big_int_Z.big_int

  val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool
 end

val string_dec : char list -> char list -> bool

val append : char list -> char list -> char list

val newline : char list

type 'a show =
  'a -> char list
  (* singleton inductive, whose constructor was Build_Show *)

val show_nat : int -> char list

val showNat : int show

val nl : char list

val trace : char list -> 'a1 -> 'a1

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

val sum_fst : (int * 'a1) list -> int

val freq_ : 'a1 g -> (int * 'a1 g) list -> 'a1 g

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

module OrderedTypeFacts :
 functor (O:OrderedType) ->
 sig
  module TO :
   sig
    type t = O.t
   end

  module IsTO :
   sig
   end

  module OrderTac :
   sig
   end

  val eq_dec : O.t -> O.t -> bool

  val lt_dec : O.t -> O.t -> bool

  val eqb : O.t -> O.t -> bool
 end

module KeyOrderedType :
 functor (O:OrderedType) ->
 sig
  module MO :
   sig
    module TO :
     sig
      type t = O.t
     end

    module IsTO :
     sig
     end

    module OrderTac :
     sig
     end

    val eq_dec : O.t -> O.t -> bool

    val lt_dec : O.t -> O.t -> bool

    val eqb : O.t -> O.t -> bool
   end
 end

module AsciiOT :
 sig
  type t = char

  val compare : t -> t -> char compare0
 end

module StringOT :
 sig
  type t = char list

  val eq_dec : char list -> char list -> bool

  type coq_SOrdering =
  | SLT
  | SEQ
  | SGT

  val coq_SOrdering_rect : 'a1 -> 'a1 -> 'a1 -> coq_SOrdering -> 'a1

  val coq_SOrdering_rec : 'a1 -> 'a1 -> 'a1 -> coq_SOrdering -> 'a1

  val strcmp : char list -> char list -> coq_SOrdering

  val compare : t -> t -> char list compare0
 end

module Raw :
 functor (X:OrderedType) ->
 sig
  module MX :
   sig
    module TO :
     sig
      type t = X.t
     end

    module IsTO :
     sig
     end

    module OrderTac :
     sig
     end

    val eq_dec : X.t -> X.t -> bool

    val lt_dec : X.t -> X.t -> bool

    val eqb : X.t -> X.t -> bool
   end

  module PX :
   sig
    module MO :
     sig
      module TO :
       sig
        type t = X.t
       end

      module IsTO :
       sig
       end

      module OrderTac :
       sig
       end

      val eq_dec : X.t -> X.t -> bool

      val lt_dec : X.t -> X.t -> bool

      val eqb : X.t -> X.t -> bool
     end
   end

  type key = X.t

  type 'elt t = (X.t * 'elt) list

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val mem : key -> 'a1 t -> bool

  val find : key -> 'a1 t -> 'a1 option

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val elements : 'a1 t -> 'a1 t

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val option_cons : key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

  val map2_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

  val map2_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

  val fold_right_pair :
    ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

  val map2_alt :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> (key * 'a3)
    list

  val at_least_one :
    'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

  val at_least_one_then_f :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2 option ->
    'a3 option
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

module Z_as_Int :
 sig
  type t = Big_int_Z.big_int

  val _0 : Big_int_Z.big_int

  val _1 : Big_int_Z.big_int

  val _2 : Big_int_Z.big_int

  val _3 : Big_int_Z.big_int

  val add : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val opp : Big_int_Z.big_int -> Big_int_Z.big_int

  val sub : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val mul : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val max : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val gt_le_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val ge_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val i2z : t -> Big_int_Z.big_int
 end

module Coq_Raw :
 functor (I:Int) ->
 functor (X:OrderedType) ->
 sig
  type key = X.t

  type 'elt tree =
  | Leaf
  | Node of 'elt tree * key * 'elt * 'elt tree * I.t

  val tree_rect :
    'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
    -> 'a1 tree -> 'a2

  val tree_rec :
    'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
    -> 'a1 tree -> 'a2

  val height : 'a1 tree -> I.t

  val cardinal : 'a1 tree -> int

  val empty : 'a1 tree

  val is_empty : 'a1 tree -> bool

  val mem : X.t -> 'a1 tree -> bool

  val find : X.t -> 'a1 tree -> 'a1 option

  val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

  val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

  val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

  val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

  val remove_min :
    'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

  val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

  val remove : X.t -> 'a1 tree -> 'a1 tree

  val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

  type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                       t_right : 'elt tree }

  val t_left : 'a1 triple -> 'a1 tree

  val t_opt : 'a1 triple -> 'a1 option

  val t_right : 'a1 triple -> 'a1 tree

  val split : X.t -> 'a1 tree -> 'a1 triple

  val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

  val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

  val elements : 'a1 tree -> (key * 'a1) list

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

  type 'elt enumeration =
  | End
  | More of key * 'elt * 'elt tree * 'elt enumeration

  val enumeration_rect :
    'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
    enumeration -> 'a2

  val enumeration_rec :
    'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
    enumeration -> 'a2

  val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

  val equal_more :
    ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
    enumeration -> bool

  val equal_cont :
    ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
    enumeration -> bool

  val equal_end : 'a1 enumeration -> bool

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

  val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

  val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

  val map2_opt :
    (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
    ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
    tree

  module Proofs :
   sig
    module MX :
     sig
      module TO :
       sig
        type t = X.t
       end

      module IsTO :
       sig
       end

      module OrderTac :
       sig
       end

      val eq_dec : X.t -> X.t -> bool

      val lt_dec : X.t -> X.t -> bool

      val eqb : X.t -> X.t -> bool
     end

    module PX :
     sig
      module MO :
       sig
        module TO :
         sig
          type t = X.t
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : X.t -> X.t -> bool

        val lt_dec : X.t -> X.t -> bool

        val eqb : X.t -> X.t -> bool
       end
     end

    module L :
     sig
      module MX :
       sig
        module TO :
         sig
          type t = X.t
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : X.t -> X.t -> bool

        val lt_dec : X.t -> X.t -> bool

        val eqb : X.t -> X.t -> bool
       end

      module PX :
       sig
        module MO :
         sig
          module TO :
           sig
            type t = X.t
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : X.t -> X.t -> bool

          val lt_dec : X.t -> X.t -> bool

          val eqb : X.t -> X.t -> bool
         end
       end

      type key = X.t

      type 'elt t = (X.t * 'elt) list

      val empty : 'a1 t

      val is_empty : 'a1 t -> bool

      val mem : key -> 'a1 t -> bool

      val find : key -> 'a1 t -> 'a1 option

      val add : key -> 'a1 -> 'a1 t -> 'a1 t

      val remove : key -> 'a1 t -> 'a1 t

      val elements : 'a1 t -> 'a1 t

      val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

      val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

      val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

      val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

      val option_cons :
        key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

      val map2_l : ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

      val map2_r : ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

      val map2 :
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

      val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

      val fold_right_pair :
        ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

      val map2_alt :
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
        (key * 'a3) list

      val at_least_one :
        'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

      val at_least_one_then_f :
        ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2 option
        -> 'a3 option
     end

    type 'elt coq_R_mem =
    | R_mem_0 of 'elt tree
    | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem
    | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem

    val coq_R_mem_rect :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
      'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
      __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1
      tree -> bool -> 'a1 coq_R_mem -> 'a2

    val coq_R_mem_rec :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
      'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
      __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1
      tree -> bool -> 'a1 coq_R_mem -> 'a2

    type 'elt coq_R_find =
    | R_find_0 of 'elt tree
    | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find
    | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find

    val coq_R_find_rect :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
      'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
      'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

    val coq_R_find_rec :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find ->
      'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2 ->
      'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

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

    val coq_R_bal_rect :
      ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
      tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
      'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 ->
      'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __
      -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ ->
      'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
      tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
      key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
      'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
      'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
      'a1 tree -> I.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree ->
      __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      'a1 tree -> 'a1 coq_R_bal -> 'a2

    val coq_R_bal_rec :
      ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
      tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
      'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 ->
      'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __
      -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ ->
      'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1
      tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree ->
      key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
      'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
      key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key ->
      'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
      'a1 tree -> I.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree ->
      __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      'a1 tree -> 'a1 coq_R_bal -> 'a2

    type 'elt coq_R_add =
    | R_add_0 of 'elt tree
    | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add
    | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add

    val coq_R_add_rect :
      key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
      -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
      coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
      -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
      coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

    val coq_R_add_rec :
      key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
      -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
      coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
      -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
      coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add -> 'a2

    type 'elt coq_R_remove_min =
    | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
    | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
       key * 'elt * 'elt tree * I.t * ('elt tree * (key * 'elt))
       * 'elt coq_R_remove_min * 'elt tree * (key * 'elt)

    val coq_R_remove_min_rect :
      ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
      -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
      -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1 tree
      -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

    val coq_R_remove_min_rec :
      ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
      -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
      -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1 tree
      -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
      ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

    type 'elt coq_R_merge =
    | R_merge_0 of 'elt tree * 'elt tree
    | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t
    | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t * 'elt tree * key * 'elt * 'elt tree * I.t * 'elt tree
       * (key * 'elt) * key * 'elt

    val coq_R_merge_rect :
      ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree -> (key * 'a1)
      -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree
      -> 'a1 coq_R_merge -> 'a2

    val coq_R_merge_rec :
      ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree -> (key * 'a1)
      -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree
      -> 'a1 coq_R_merge -> 'a2

    type 'elt coq_R_remove =
    | R_remove_0 of 'elt tree
    | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove
    | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove

    val coq_R_remove_rect :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
      'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 ->
      'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

    val coq_R_remove_rec :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove ->
      'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t
      -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 ->
      'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

    type 'elt coq_R_concat =
    | R_concat_0 of 'elt tree * 'elt tree
    | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t
    | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * (key * 'elt)

    val coq_R_concat_rect :
      ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree -> (key * 'a1)
      -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat
      -> 'a2

    val coq_R_concat_rec :
      ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1
      tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree -> (key * 'a1)
      -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_concat
      -> 'a2

    type 'elt coq_R_split =
    | R_split_0 of 'elt tree
    | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree
    | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree

    val coq_R_split_rect :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
      -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2)
      -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __
      -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option
      -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split
      -> 'a2

    val coq_R_split_rec :
      X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
      -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1 tree
      -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2)
      -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __
      -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option
      -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split
      -> 'a2

    type ('elt, 'x) coq_R_map_option =
    | R_map_option_0 of 'elt tree
    | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
       * ('elt, 'x) coq_R_map_option
    | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
       * ('elt, 'x) coq_R_map_option

    val coq_R_map_option_rect :
      (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
      'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
      tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
      coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
      coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
      'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
      'a3

    val coq_R_map_option_rec :
      (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
      'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
      tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
      coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key -> 'a1
      -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
      coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
      'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
      'a3

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

    val coq_R_map2_opt_rect :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
      tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
      __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t -> __ ->
      'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree ->
      ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
      coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree ->
      key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
      tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ ->
      'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
      'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3
      tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

    val coq_R_map2_opt_rec :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
      tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
      __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1
      tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t -> __ ->
      'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3 tree ->
      ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
      coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree ->
      key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
      tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ ->
      'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
      'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3
      tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

    val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

    val flatten_e : 'a1 enumeration -> (key * 'a1) list
   end
 end

module IntMake :
 functor (I:Int) ->
 functor (X:OrderedType) ->
 sig
  module E :
   sig
    type t = X.t

    val compare : t -> t -> t compare0

    val eq_dec : t -> t -> bool
   end

  module Raw :
   sig
    type key = X.t

    type 'elt tree =
    | Leaf
    | Node of 'elt tree * key * 'elt * 'elt tree * I.t

    val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2

    val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2

    val height : 'a1 tree -> I.t

    val cardinal : 'a1 tree -> int

    val empty : 'a1 tree

    val is_empty : 'a1 tree -> bool

    val mem : X.t -> 'a1 tree -> bool

    val find : X.t -> 'a1 tree -> 'a1 option

    val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

    val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

    val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

    val remove : X.t -> 'a1 tree -> 'a1 tree

    val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                         t_right : 'elt tree }

    val t_left : 'a1 triple -> 'a1 tree

    val t_opt : 'a1 triple -> 'a1 option

    val t_right : 'a1 triple -> 'a1 tree

    val split : X.t -> 'a1 tree -> 'a1 triple

    val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

    val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

    val elements : 'a1 tree -> (key * 'a1) list

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

    type 'elt enumeration =
    | End
    | More of key * 'elt * 'elt tree * 'elt enumeration

    val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

    val equal_more :
      ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool

    val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool

    val equal_end : 'a1 enumeration -> bool

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

    val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

    val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree

    module Proofs :
     sig
      module MX :
       sig
        module TO :
         sig
          type t = X.t
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : X.t -> X.t -> bool

        val lt_dec : X.t -> X.t -> bool

        val eqb : X.t -> X.t -> bool
       end

      module PX :
       sig
        module MO :
         sig
          module TO :
           sig
            type t = X.t
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : X.t -> X.t -> bool

          val lt_dec : X.t -> X.t -> bool

          val eqb : X.t -> X.t -> bool
         end
       end

      module L :
       sig
        module MX :
         sig
          module TO :
           sig
            type t = X.t
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : X.t -> X.t -> bool

          val lt_dec : X.t -> X.t -> bool

          val eqb : X.t -> X.t -> bool
         end

        module PX :
         sig
          module MO :
           sig
            module TO :
             sig
              type t = X.t
             end

            module IsTO :
             sig
             end

            module OrderTac :
             sig
             end

            val eq_dec : X.t -> X.t -> bool

            val lt_dec : X.t -> X.t -> bool

            val eqb : X.t -> X.t -> bool
           end
         end

        type key = X.t

        type 'elt t = (X.t * 'elt) list

        val empty : 'a1 t

        val is_empty : 'a1 t -> bool

        val mem : key -> 'a1 t -> bool

        val find : key -> 'a1 t -> 'a1 option

        val add : key -> 'a1 -> 'a1 t -> 'a1 t

        val remove : key -> 'a1 t -> 'a1 t

        val elements : 'a1 t -> 'a1 t

        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

        val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

        val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

        val option_cons :
          key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

        val map2_l :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

        val map2_r :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

        val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

        val fold_right_pair :
          ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

        val map2_alt :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
          (key * 'a3) list

        val at_least_one :
          'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

        val at_least_one_then_f :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
          option -> 'a3 option
       end

      type 'elt coq_R_mem =
      | R_mem_0 of 'elt tree
      | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
         * bool * 'elt coq_R_mem
      | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
      | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
         * bool * 'elt coq_R_mem

      val coq_R_mem_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

      val coq_R_mem_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

      type 'elt coq_R_find =
      | R_find_0 of 'elt tree
      | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt option * 'elt coq_R_find
      | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
      | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt option * 'elt coq_R_find

      val coq_R_find_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

      val coq_R_find_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2

      type 'elt coq_R_bal =
      | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t
      | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t
      | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
         I.t
      | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t
      | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t
      | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
         I.t
      | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

      val coq_R_bal_rect :
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
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      val coq_R_bal_rec :
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
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      type 'elt coq_R_add =
      | R_add_0 of 'elt tree
      | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
         * 'elt tree * 'elt coq_R_add
      | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
      | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
         * 'elt tree * 'elt coq_R_add

      val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2

      val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2

      type 'elt coq_R_remove_min =
      | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
      | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
         * key * 'elt * 'elt tree * I.t * ('elt tree * (key * 'elt))
         * 'elt coq_R_remove_min * 'elt tree * (key * 'elt)

      val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1
        tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

      val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2 -> 'a1
        tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min -> 'a2

      type 'elt coq_R_merge =
      | R_merge_0 of 'elt tree * 'elt tree
      | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * I.t
      | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * I.t
         * 'elt tree * (key * 'elt) * key * 'elt

      val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1 tree
        -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      type 'elt coq_R_remove =
      | R_remove_0 of 'elt tree
      | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt tree * 'elt coq_R_remove
      | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
      | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt tree * 'elt coq_R_remove

      val coq_R_remove_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

      val coq_R_remove_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2

      type 'elt coq_R_concat =
      | R_concat_0 of 'elt tree * 'elt tree
      | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * I.t
      | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * I.t
         * 'elt tree * (key * 'elt)

      val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_concat -> 'a2

      val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_concat -> 'a2

      type 'elt coq_R_split =
      | R_split_0 of 'elt tree
      | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option
         * 'elt tree
      | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
      | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
         I.t * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option
         * 'elt tree

      val coq_R_split_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2

      val coq_R_split_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2

      type ('elt, 'x) coq_R_map_option =
      | R_map_option_0 of 'elt tree
      | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * I.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
         * ('elt, 'x) coq_R_map_option
      | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * I.t * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
         * ('elt, 'x) coq_R_map_option

      val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3

      val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3

      type ('elt, 'x0, 'x) coq_R_map2_opt =
      | R_map2_opt_0 of 'elt tree * 'x0 tree
      | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * I.t
      | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * I.t * 'x0 tree * key * 'x0 * 'x0 tree * I.t * 
         'x0 tree * 'x0 option * 'x0 tree * 'x * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt
      | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * I.t * 'x0 tree * key * 'x0 * 'x0 tree * I.t * 
         'x0 tree * 'x0 option * 'x0 tree * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt

      val coq_R_map2_opt_rect :
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
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val coq_R_map2_opt_rec :
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
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

      val flatten_e : 'a1 enumeration -> (key * 'a1) list
     end
   end

  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)

  val this : 'a1 bst -> 'a1 Raw.tree

  type 'elt t = 'elt bst

  type key = E.t

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val mem : key -> 'a1 t -> bool

  val find : key -> 'a1 t -> 'a1 option

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end

module Make :
 functor (X:OrderedType) ->
 sig
  module E :
   sig
    type t = X.t

    val compare : t -> t -> t compare0

    val eq_dec : t -> t -> bool
   end

  module Raw :
   sig
    type key = X.t

    type 'elt tree =
    | Leaf
    | Node of 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t

    val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> Z_as_Int.t
      -> 'a2) -> 'a1 tree -> 'a2

    val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> Z_as_Int.t
      -> 'a2) -> 'a1 tree -> 'a2

    val height : 'a1 tree -> Z_as_Int.t

    val cardinal : 'a1 tree -> int

    val empty : 'a1 tree

    val is_empty : 'a1 tree -> bool

    val mem : X.t -> 'a1 tree -> bool

    val find : X.t -> 'a1 tree -> 'a1 option

    val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

    val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

    val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

    val remove : X.t -> 'a1 tree -> 'a1 tree

    val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                         t_right : 'elt tree }

    val t_left : 'a1 triple -> 'a1 tree

    val t_opt : 'a1 triple -> 'a1 option

    val t_right : 'a1 triple -> 'a1 tree

    val split : X.t -> 'a1 tree -> 'a1 triple

    val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

    val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

    val elements : 'a1 tree -> (key * 'a1) list

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

    type 'elt enumeration =
    | End
    | More of key * 'elt * 'elt tree * 'elt enumeration

    val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

    val equal_more :
      ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool

    val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool

    val equal_end : 'a1 enumeration -> bool

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

    val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

    val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree

    module Proofs :
     sig
      module MX :
       sig
        module TO :
         sig
          type t = X.t
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : X.t -> X.t -> bool

        val lt_dec : X.t -> X.t -> bool

        val eqb : X.t -> X.t -> bool
       end

      module PX :
       sig
        module MO :
         sig
          module TO :
           sig
            type t = X.t
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : X.t -> X.t -> bool

          val lt_dec : X.t -> X.t -> bool

          val eqb : X.t -> X.t -> bool
         end
       end

      module L :
       sig
        module MX :
         sig
          module TO :
           sig
            type t = X.t
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : X.t -> X.t -> bool

          val lt_dec : X.t -> X.t -> bool

          val eqb : X.t -> X.t -> bool
         end

        module PX :
         sig
          module MO :
           sig
            module TO :
             sig
              type t = X.t
             end

            module IsTO :
             sig
             end

            module OrderTac :
             sig
             end

            val eq_dec : X.t -> X.t -> bool

            val lt_dec : X.t -> X.t -> bool

            val eqb : X.t -> X.t -> bool
           end
         end

        type key = X.t

        type 'elt t = (X.t * 'elt) list

        val empty : 'a1 t

        val is_empty : 'a1 t -> bool

        val mem : key -> 'a1 t -> bool

        val find : key -> 'a1 t -> 'a1 option

        val add : key -> 'a1 -> 'a1 t -> 'a1 t

        val remove : key -> 'a1 t -> 'a1 t

        val elements : 'a1 t -> 'a1 t

        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

        val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

        val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

        val option_cons :
          key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

        val map2_l :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

        val map2_r :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

        val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

        val fold_right_pair :
          ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

        val map2_alt :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
          (key * 'a3) list

        val at_least_one :
          'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

        val at_least_one_then_f :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
          option -> 'a3 option
       end

      type 'elt coq_R_mem =
      | R_mem_0 of 'elt tree
      | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * bool * 'elt coq_R_mem
      | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t
      | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * bool * 'elt coq_R_mem

      val coq_R_mem_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

      val coq_R_mem_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2

      type 'elt coq_R_find =
      | R_find_0 of 'elt tree
      | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt option * 'elt coq_R_find
      | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt option * 'elt coq_R_find

      val coq_R_find_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
        coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option ->
        'a1 coq_R_find -> 'a2

      val coq_R_find_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 option -> 'a1
        coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option ->
        'a1 coq_R_find -> 'a2

      type 'elt coq_R_bal =
      | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

      val coq_R_bal_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
        -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      val coq_R_bal_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
        -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      type 'elt coq_R_add =
      | R_add_0 of 'elt tree
      | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_add
      | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t
      | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_add

      val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_add -> 'a2

      val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_add -> 'a2

      type 'elt coq_R_remove_min =
      | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
      | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
         * key * 'elt * 'elt tree * Z_as_Int.t * ('elt tree * (key * 'elt))
         * 'elt coq_R_remove_min * 'elt tree * (key * 'elt)

      val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2

      val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2

      type 'elt coq_R_merge =
      | R_merge_0 of 'elt tree * 'elt tree
      | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt

      val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) ->
        'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) ->
        'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      type 'elt coq_R_remove =
      | R_remove_0 of 'elt tree
      | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_remove
      | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_remove

      val coq_R_remove_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2

      val coq_R_remove_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2

      type 'elt coq_R_concat =
      | R_concat_0 of 'elt tree * 'elt tree
      | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * (key * 'elt)

      val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

      val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

      type 'elt coq_R_split =
      | R_split_0 of 'elt tree
      | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree
      | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree

      val coq_R_split_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

      val coq_R_split_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

      type ('elt, 'x) coq_R_map_option =
      | R_map_option_0 of 'elt tree
      | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option * 
         'x tree * ('elt, 'x) coq_R_map_option
      | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
         * ('elt, 'x) coq_R_map_option

      val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 -> __
        -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3

      val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 -> __
        -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3

      type ('elt, 'x0, 'x) coq_R_map2_opt =
      | R_map2_opt_0 of 'elt tree * 'x0 tree
      | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
         * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt
      | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
         * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt

      val coq_R_map2_opt_rect :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __
        -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
        'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
        'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree ->
        ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val coq_R_map2_opt_rec :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __
        -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
        'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
        'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree ->
        ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

      val flatten_e : 'a1 enumeration -> (key * 'a1) list
     end
   end

  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)

  val this : 'a1 bst -> 'a1 Raw.tree

  type 'elt t = 'elt bst

  type key = E.t

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val mem : key -> 'a1 t -> bool

  val find : key -> 'a1 t -> 'a1 option

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end

module Map :
 sig
  module E :
   sig
    type t = char list

    val compare : char list -> char list -> char list compare0

    val eq_dec : char list -> char list -> bool
   end

  module Raw :
   sig
    type key = char list

    type 'elt tree =
    | Leaf
    | Node of 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t

    val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> Z_as_Int.t
      -> 'a2) -> 'a1 tree -> 'a2

    val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> Z_as_Int.t
      -> 'a2) -> 'a1 tree -> 'a2

    val height : 'a1 tree -> Z_as_Int.t

    val cardinal : 'a1 tree -> int

    val empty : 'a1 tree

    val is_empty : 'a1 tree -> bool

    val mem : char list -> 'a1 tree -> bool

    val find : char list -> 'a1 tree -> 'a1 option

    val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    val add : key -> 'a1 -> 'a1 tree -> 'a1 tree

    val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree * (key * 'a1)

    val merge : 'a1 tree -> 'a1 tree -> 'a1 tree

    val remove : char list -> 'a1 tree -> 'a1 tree

    val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree

    type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                         t_right : 'elt tree }

    val t_left : 'a1 triple -> 'a1 tree

    val t_opt : 'a1 triple -> 'a1 option

    val t_right : 'a1 triple -> 'a1 tree

    val split : char list -> 'a1 tree -> 'a1 triple

    val concat : 'a1 tree -> 'a1 tree -> 'a1 tree

    val elements_aux : (key * 'a1) list -> 'a1 tree -> (key * 'a1) list

    val elements : 'a1 tree -> (key * 'a1) list

    val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

    type 'elt enumeration =
    | End
    | More of key * 'elt * 'elt tree * 'elt enumeration

    val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2

    val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration

    val equal_more :
      ('a1 -> 'a1 -> bool) -> char list -> 'a1 -> ('a1 enumeration -> bool)
      -> 'a1 enumeration -> bool

    val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool

    val equal_end : 'a1 enumeration -> bool

    val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool

    val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

    val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

    val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree

    val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree

    module Proofs :
     sig
      module MX :
       sig
        module TO :
         sig
          type t = char list
         end

        module IsTO :
         sig
         end

        module OrderTac :
         sig
         end

        val eq_dec : char list -> char list -> bool

        val lt_dec : char list -> char list -> bool

        val eqb : char list -> char list -> bool
       end

      module PX :
       sig
        module MO :
         sig
          module TO :
           sig
            type t = char list
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : char list -> char list -> bool

          val lt_dec : char list -> char list -> bool

          val eqb : char list -> char list -> bool
         end
       end

      module L :
       sig
        module MX :
         sig
          module TO :
           sig
            type t = char list
           end

          module IsTO :
           sig
           end

          module OrderTac :
           sig
           end

          val eq_dec : char list -> char list -> bool

          val lt_dec : char list -> char list -> bool

          val eqb : char list -> char list -> bool
         end

        module PX :
         sig
          module MO :
           sig
            module TO :
             sig
              type t = char list
             end

            module IsTO :
             sig
             end

            module OrderTac :
             sig
             end

            val eq_dec : char list -> char list -> bool

            val lt_dec : char list -> char list -> bool

            val eqb : char list -> char list -> bool
           end
         end

        type key = char list

        type 'elt t = (char list * 'elt) list

        val empty : 'a1 t

        val is_empty : 'a1 t -> bool

        val mem : key -> 'a1 t -> bool

        val find : key -> 'a1 t -> 'a1 option

        val add : key -> 'a1 -> 'a1 t -> 'a1 t

        val remove : key -> 'a1 t -> 'a1 t

        val elements : 'a1 t -> 'a1 t

        val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

        val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

        val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

        val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

        val option_cons :
          key -> 'a1 option -> (key * 'a1) list -> (key * 'a1) list

        val map2_l :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t

        val map2_r :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t

        val map2 :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

        val combine : 'a1 t -> 'a2 t -> ('a1 option * 'a2 option) t

        val fold_right_pair :
          ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1 * 'a2) list -> 'a3 -> 'a3

        val map2_alt :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t ->
          (key * 'a3) list

        val at_least_one :
          'a1 option -> 'a2 option -> ('a1 option * 'a2 option) option

        val at_least_one_then_f :
          ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2
          option -> 'a3 option
       end

      type 'elt coq_R_mem =
      | R_mem_0 of 'elt tree
      | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * bool * 'elt coq_R_mem
      | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t
      | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * bool * 'elt coq_R_mem

      val coq_R_mem_rect :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
        bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
        coq_R_mem -> 'a2

      val coq_R_mem_rec :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> bool -> 'a1
        coq_R_mem -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
        bool -> 'a1 coq_R_mem -> 'a2 -> 'a2) -> 'a1 tree -> bool -> 'a1
        coq_R_mem -> 'a2

      type 'elt coq_R_find =
      | R_find_0 of 'elt tree
      | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt option * 'elt coq_R_find
      | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt option * 'elt coq_R_find

      val coq_R_find_rect :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 option ->
        'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
        'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option
        -> 'a1 coq_R_find -> 'a2

      val coq_R_find_rec :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 option ->
        'a1 coq_R_find -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ ->
        'a1 option -> 'a1 coq_R_find -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option
        -> 'a1 coq_R_find -> 'a2

      type 'elt coq_R_bal =
      | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
      | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t
      | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key
         * 'elt * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_bal_8 of 'elt tree * key * 'elt * 'elt tree

      val coq_R_bal_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
        -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      val coq_R_bal_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree ->
        key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1
        -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree
        -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2

      type 'elt coq_R_add =
      | R_add_0 of 'elt tree
      | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_add
      | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * Z_as_Int.t
      | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_add

      val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_add -> 'a2

      val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1
        coq_R_add -> 'a2

      type 'elt coq_R_remove_min =
      | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
      | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree
         * key * 'elt * 'elt tree * Z_as_Int.t * ('elt tree * (key * 'elt))
         * 'elt coq_R_remove_min * 'elt tree * (key * 'elt)

      val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2

      val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2 -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> ('a1 tree * (key * 'a1)) -> 'a1 coq_R_remove_min
        -> 'a2

      type 'elt coq_R_merge =
      | R_merge_0 of 'elt tree * 'elt tree
      | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * (key * 'elt) * key * 'elt

      val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) ->
        'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> key -> 'a1 -> __ -> 'a2) ->
        'a1 tree -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2

      type 'elt coq_R_remove =
      | R_remove_0 of 'elt tree
      | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_remove
      | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * 'elt coq_R_remove

      val coq_R_remove_rect :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2

      val coq_R_remove_rec :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_remove -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1
        tree -> 'a1 coq_R_remove -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_remove -> 'a2

      type 'elt coq_R_concat =
      | R_concat_0 of 'elt tree * 'elt tree
      | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt tree * (key * 'elt)

      val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

      val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2) ->
        ('a1 tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        Z_as_Int.t -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a1 tree -> (key * 'a1) -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_concat -> 'a2

      type 'elt coq_R_split =
      | R_split_0 of 'elt tree
      | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree
      | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t
      | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'elt triple * 'elt coq_R_split * 'elt tree
         * 'elt option * 'elt tree

      val coq_R_split_rect :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple ->
        'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

      val coq_R_split_rec :
        char list -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple ->
        'a1 coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> Z_as_Int.t -> __ -> __ -> __ -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ ->
        'a2) -> 'a1 tree -> 'a1 triple -> 'a1 coq_R_split -> 'a2

      type ('elt, 'x) coq_R_map_option =
      | R_map_option_0 of 'elt tree
      | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'x * 'x tree * ('elt, 'x) coq_R_map_option * 
         'x tree * ('elt, 'x) coq_R_map_option
      | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree
         * Z_as_Int.t * 'x tree * ('elt, 'x) coq_R_map_option * 'x tree
         * ('elt, 'x) coq_R_map_option

      val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 -> __
        -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3

      val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 -> __
        -> 'a2 tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree
        -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> __ -> 'a2 tree ->
        ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3

      type ('elt, 'x0, 'x) coq_R_map2_opt =
      | R_map2_opt_0 of 'elt tree * 'x0 tree
      | R_map2_opt_1 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t
      | R_map2_opt_2 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
         * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt
      | R_map2_opt_3 of 'elt tree * 'x0 tree * 'elt tree * key * 'elt
         * 'elt tree * Z_as_Int.t * 'x0 tree * key * 'x0 * 'x0 tree
         * Z_as_Int.t * 'x0 tree * 'x0 option * 'x0 tree * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt * 'x tree
         * ('elt, 'x0, 'x) coq_R_map2_opt

      val coq_R_map2_opt_rect :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __
        -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
        'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
        'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree ->
        ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val coq_R_map2_opt_rec :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> Z_as_Int.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2
        tree -> Z_as_Int.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __
        -> 'a3 -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 ->
        'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> Z_as_Int.t
        -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> Z_as_Int.t -> __ ->
        'a2 tree -> 'a2 option -> 'a2 tree -> __ -> __ -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1, 'a2, 'a3)
        coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree -> 'a2 tree -> 'a3 tree ->
        ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4

      val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2

      val flatten_e : 'a1 enumeration -> (key * 'a1) list
     end
   end

  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)

  val this : 'a1 bst -> 'a1 Raw.tree

  type 'elt t = 'elt bst

  type key = char list

  val empty : 'a1 t

  val is_empty : 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val mem : key -> 'a1 t -> bool

  val find : key -> 'a1 t -> 'a1 option

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map2 :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (key * 'a1) list

  val cardinal : 'a1 t -> int

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool
 end

type state = { maxSuccessTests : int; maxDiscardedTests : int;
               maxShrinkNo : int; computeSize : (int -> int -> int);
               numSuccessTests : int; numDiscardedTests : int;
               labels : int Map.t; expectedFailure : bool;
               randomSeed0 : randomSeed; numSuccessShrinks : int;
               numTryShrinks : int; stDoAnalysis : bool }

val updSuccTests : state -> (int -> int) -> state

val updDiscTests : state -> (int -> int) -> state

type 'a genSized =
  int -> 'a g
  (* singleton inductive, whose constructor was Build_GenSized *)

type 'a gen = 'a g
  (* singleton inductive, whose constructor was Build_Gen *)

type 'a fuzzy =
  'a -> 'a g
  (* singleton inductive, whose constructor was Build_Fuzzy *)

val genOfGenSized : 'a1 genSized -> 'a1 gen

val gte : int -> int -> bool

type args = { replay : (randomSeed * int) option; maxSuccess : int;
              maxDiscard : int; maxShrinks : int; maxSize : int;
              chatty : bool; analysis : bool }

val updMaxSuccess : args -> int -> args

val updMaxDiscard : args -> int -> args

val updAnalysis : args -> bool -> args

type result =
| Success of int * int * (char list * int) list * char list
| GaveUp of int * (char list * int) list * char list
| Failure of int * int * int * randomSeed * int * char list
   * (char list * int) list * char list
| NoExpectedFailure of int * (char list * int) list * char list

val defNumTests : int

val defNumDiscards : int

val defNumShrinks : int

val defSize : int

val doAnalysis : bool

val stdArgs : args

val roundTo : int -> int -> int

val computeSize'' : int -> int -> int -> int -> int

val computeSize' : args -> int -> int -> int

val insertBy : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val insSortBy : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list

val summary : state -> (char list * int) list

val doneTesting : state -> result

val giveUp : state -> result

type oCamlFloat = float

val showOCamlFloat' : oCamlFloat -> char list

val showOCamlFloat : oCamlFloat show

type 'a timedResult =
| TResult of 'a * oCamlFloat * oCamlFloat * oCamlFloat

val withTime : (unit -> 'a1) -> 'a1 timedResult

val showTimedResult : 'a1 show -> 'a1 timedResult show

val showCollectStatistics : (char list * int) list -> char list

val showResult : result show

val withInstrumentation : (unit -> bool option) -> bool option * (bool * int)

val random_fuel : int

val pick_next_aux :
  int -> 'a1 g -> ('a1 -> 'a1 g) -> (int * 'a1) list -> (int * 'a1) list ->
  (int * 'a1) list -> (int * 'a1) list -> int -> (int * 'a1) list -> ((((('a1
  g * (int * 'a1) list) * (int * 'a1) list) * (int * 'a1) list) * (int * 'a1)
  list) * int) * (int * 'a1) list

val pick_fuel : int

val pick_next :
  'a1 g -> ('a1 -> 'a1 g) -> (int * 'a1) list -> (int * 'a1) list ->
  (int * 'a1) list -> (int * 'a1) list -> int -> (int * 'a1) list -> ((((('a1
  g * (int * 'a1) list) * (int * 'a1) list) * (int * 'a1) list) * (int * 'a1)
  list) * int) * (int * 'a1) list

val printnvb : unit -> int

val doneTestingFuzz : int -> state -> result

val clear_queues : int -> bool

val fuzzLoopAux :
  int -> state -> (int * 'a1) list -> (int * 'a1) list -> (int * 'a1) list ->
  (int * 'a1) list -> int -> (int * 'a1) list -> 'a1 g -> ('a1 -> 'a1 g) ->
  ('a1 -> char list) -> ('a1 -> bool option) -> result

val fuzzLoopWith :
  args -> 'a1 g -> ('a1 -> 'a1 g) -> ('a1 -> char list) -> ('a1 -> bool
  option) -> result

val print_extracted_coq_string : char list -> unit

type dec = decidable
  (* singleton inductive, whose constructor was Build_Dec *)

type ('a, 'b) implies = 'a -> 'b -> bool option

val implies0 : ('a1, 'a2) implies -> 'a1 -> 'a2 -> bool option

val impliesOO : (bool option, bool option) implies

val impliesBO : (bool, bool option) implies

val genBoolSized : bool genSized

val genNatSized : int genSized

val monad_option : __ option monad

type typ =
| TBool
| TFun of typ * typ

val dec_type : typ -> typ -> dec

val showTyp : typ show

type expr =
| Var of int
| Bool of bool
| Abs of typ * expr
| App of expr * expr

val sexp : bool -> char list

val showExpr : expr show

type ctx = typ list

val getTyp : ctx -> expr -> typ option

val typeCheck : ctx -> expr -> typ -> bool

val shift : Big_int_Z.big_int -> expr -> expr

val subst : int -> expr -> expr -> expr

val substTop : expr -> expr -> expr

val fromMaybe : 'a1 -> 'a1 option -> 'a1

val pstep : expr -> expr option

val multistep : int -> (expr -> expr option) -> expr -> expr option

val mt : expr -> typ option

val isJust : 'a1 option -> bool

val mtypeCheck : expr option -> typ -> bool

val prop_SinglePreserve : expr -> bool option

val prop_MultiPreserve : expr -> bool option

val genSizedTyp : typ genSized

val fuzzyTyp : typ fuzzy

val fuzzynat : int fuzzy

val fuzzybool : bool fuzzy

val genSizedExpr : expr genSized

val fuzzyExpr : expr fuzzy

val num_tests : int

val test_prop_SinglePreserve : expr -> bool option

val test_prop_SinglePreserve_fuzzer : unit -> result

val test_prop_MultiPreserve : expr -> bool option

val test_prop_MultiPreserve_fuzzer : unit -> result

val qctest_test_prop_SinglePreserve : unit -> unit

val qctest_test_prop_MultiPreserve : unit -> unit

type oCamlString = string

val qctest_map : oCamlString -> unit

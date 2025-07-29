#lang racket

(require "../src/Impl.rkt")
(require "../src/Spec.rkt")
(require "../src/Generation.rkt")
(require rackcheck)
(require rackunit)
(provide (all-defined-out))

#| Validity Properties |#

(define sample_InsertValid
    (gen:let ([t bespoke] [k gen:natural] [v gen:natural])
        (list t k v))
)

(define sample_DeleteValid
    (gen:let ([t bespoke] [k gen:natural])
        (list t k))
)

(define sample_UnionValid
    (gen:let ([t1 bespoke] [t2 bespoke])
        (list t1 t2))
)

#| Post-condition Properties |#

(define sample_InsertPost
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)

(define sample_DeletePost
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural])
        (list t k1 k2))
)

(define sample_UnionPost
    (gen:let ([t1 bespoke] [t2 bespoke] [k gen:natural])
        (list t1 t2 k))
)

#| Model-based Properties |#

(define sample_InsertModel
    (gen:let ([t bespoke] [k gen:natural] [v gen:natural])
        (list t k v))
)

(define sample_DeleteModel
    (gen:let ([t bespoke] [k gen:natural])
        (list t k))
)

(define sample_UnionModel
    (gen:let ([t1 bespoke] [t2 bespoke])
        (list t1 t2))
)

#| Metamorphic Properties |#

(define sample_InsertInsert
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v1 gen:natural] [v2 gen:natural])
        (list t k1 k2 v1 v2))
)

(define sample_InsertDelete
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)

(define sample_InsertUnion
    (gen:let ([t1 bespoke] [t2 bespoke] [k gen:natural] [v gen:natural])
        (list t1 t2 k v))
)

(define sample_DeleteInsert
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)

(define sample_DeleteDelete
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural])
        (list t k1 k2))
)

(define sample_DeleteUnion
    (gen:let ([t1 bespoke] [t2 bespoke] [k gen:natural])
        (list t1 t2 k))
)

(define sample_UnionDeleteInsert
    (gen:let ([t1 bespoke] [t2 bespoke] [k gen:natural] [v gen:natural])
        (list t1 t2 k v))
)

(define sample_UnionUnionIdem
    (gen:let ([t bespoke])
        t)
)

(define sample_UnionUnionAssoc
    (gen:let ([t1 bespoke] [t2 bespoke] [t3 bespoke])
        (list t1 t2 t3))
)

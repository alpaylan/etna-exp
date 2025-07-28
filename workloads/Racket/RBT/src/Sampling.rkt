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


#| Post-condition Properties |#

(define sample_InsertPost
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)

(define sample_DeletePost
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural])
        (list t k1 k2))
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


#| Metamorphic Properties |#

(define sample_InsertInsert
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v1 gen:natural] [v2 gen:natural])
        (list t k1 k2 v1 v2))
)

(define sample_InsertDelete
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)


(define sample_DeleteInsert
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural] [v gen:natural])
        (list t k1 k2 v))
)

(define sample_DeleteDelete
    (gen:let ([t bespoke] [k1 gen:natural] [k2 gen:natural])
        (list t k1 k2))
)





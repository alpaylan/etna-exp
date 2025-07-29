#lang racket

(require "../src/Impl.rkt")
(require "../src/Spec.rkt")
(require "../src/Generation.rkt")
(require rackcheck)
(require rackunit)
(provide (all-defined-out))
#| Validity Properties |#

(define sample_SinglePreserve
    (gen:let ([e gSized])
        e)
)

(define sample_MultiPreserve
    (gen:let ([e gSized])
        e)
)


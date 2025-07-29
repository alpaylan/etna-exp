#lang racket

(require (only-in "Impl.rkt" E T))
(require (only-in rackcheck gen:tuple gen:natural gen:integer-in gen:list gen:let))

(define (insert_correct kv t)
  (let ([k (first kv)])
    (let([v (second kv)])
      (match t
        [(E) (T (E) k v (E))]
        [(T l k2 v2 r) (cond [(< k k2) (T (insert_correct kv l) k2 v2 r)]
                             [(> k k2) (T l k2 v2 (insert_correct kv r))]
                             [else (T l k2 v r)])]))))

(define gen:kv (gen:tuple (gen:integer-in 0 40) (gen:integer-in 0 40)))

(define gen:kvlist (gen:list gen:kv #:max-length 10))

(define bespoke
  (gen:let ([kvs gen:kvlist])
           (foldl insert_correct (E) kvs)))

(provide bespoke)
#lang racket/base

(module+ main
  (require racket/cmdline)
  (require rackcheck)
  (require racket/dict)
  (require (prefix-in rc: "Strategies/RackcheckBespoke.rkt"))
  (require (prefix-in pl: "Strategies/ProplangBespoke.rkt"))
  (require (prefix-in prl: "Strategies/ParallelBespoke.rkt"))

  (command-line
   #:program "rackcheck-bespoke"
   #:args info

   (define property (list-ref info 0))
   (define strategy-longform (list-ref info 1))
   (define strategy (case strategy-longform
                      [("RackcheckBespoke" "rc") "rc"]
                      [("ProplangBespoke" "pl") "pl"]
                      [("ParallelBespoke" "prl") "prl"]
                      (else (error "Unknown strategy"))))

   (define search-key (string-append strategy ":" property))
   ; Dynamically load the property from the strategy file

   (define tests 4000000)
   (define config (make-config #:tests tests #:deadline (+ (current-inexact-milliseconds) (* 240 1000))))

   (define (check-rackcheck-property p) (check-property config p))
   (define (check-tartarus-property p) (p tests))

   (define checker-fn (case strategy
                        [("rc") check-rackcheck-property]
                        [("pl") check-tartarus-property]
                        [("prl") check-tartarus-property]
                        (else (error "Unknown strategy"))))

   (define props
     `(
       ; Rackcheck properties
       ("rc:InsertValid"        . ,rc:test_prop_InsertValid)
       ("rc:DeleteValid"        . ,rc:test_prop_DeleteValid)
       ("rc:UnionValid"         . ,rc:test_prop_UnionValid)
       ("rc:InsertPost"         . ,rc:test_prop_InsertPost)
       ("rc:DeletePost"         . ,rc:test_prop_DeletePost)
       ("rc:UnionPost"          . ,rc:test_prop_UnionPost)
       ("rc:InsertModel"        . ,rc:test_prop_InsertModel)
       ("rc:DeleteModel"        . ,rc:test_prop_DeleteModel)
       ("rc:UnionModel"         . ,rc:test_prop_UnionModel)
       ("rc:InsertInsert"       . ,rc:test_prop_InsertInsert)
       ("rc:InsertDelete"       . ,rc:test_prop_InsertDelete)
       ("rc:InsertUnion"        . ,rc:test_prop_InsertUnion)
       ("rc:DeleteInsert"       . ,rc:test_prop_DeleteInsert)
       ("rc:DeleteDelete"       . ,rc:test_prop_DeleteDelete)
       ("rc:DeleteUnion"        . ,rc:test_prop_DeleteUnion)
       ("rc:UnionDeleteInsert"  . ,rc:test_prop_UnionDeleteInsert)
       ("rc:UnionUnionIdem"     . ,rc:test_prop_UnionUnionIdem)
       ("rc:UnionUnionAssoc"    . ,rc:test_prop_UnionUnionAssoc)
       ; Proplang properties
       ("pl:InsertValid"        . ,pl:test_prop_InsertValid)
       ("pl:DeleteValid"        . ,pl:test_prop_DeleteValid)
       ("pl:UnionValid"         . ,pl:test_prop_UnionValid)
       ("pl:InsertPost"         . ,pl:test_prop_InsertPost)
       ("pl:DeletePost"         . ,pl:test_prop_DeletePost)
       ("pl:UnionPost"          . ,pl:test_prop_UnionPost)
       ("pl:InsertModel"        . ,pl:test_prop_InsertModel)
       ("pl:DeleteModel"        . ,pl:test_prop_DeleteModel)
       ("pl:UnionModel"         . ,pl:test_prop_UnionModel)
       ("pl:InsertInsert"       . ,pl:test_prop_InsertInsert)
       ("pl:InsertDelete"       . ,pl:test_prop_InsertDelete)
       ("pl:InsertUnion"        . ,pl:test_prop_InsertUnion)
       ("pl:DeleteInsert"       . ,pl:test_prop_DeleteInsert)
       ("pl:DeleteDelete"       . ,pl:test_prop_DeleteDelete)
       ("pl:DeleteUnion"        . ,pl:test_prop_DeleteUnion)
       ("pl:UnionDeleteInsert"  . ,pl:test_prop_UnionDeleteInsert)
       ("pl:UnionUnionIdem"     . ,pl:test_prop_UnionUnionIdem)
       ("pl:UnionUnionAssoc"    . ,pl:test_prop_UnionUnionAssoc)
       ; Parallel proplang properties
       ("prl:InsertValid"        . ,prl:test_prop_InsertValid)
       ("prl:DeleteValid"        . ,prl:test_prop_DeleteValid)
       ("prl:UnionValid"         . ,prl:test_prop_UnionValid)
       ("prl:InsertPost"         . ,prl:test_prop_InsertPost)
       ("prl:DeletePost"         . ,prl:test_prop_DeletePost)
       ("prl:UnionPost"          . ,prl:test_prop_UnionPost)
       ("prl:InsertModel"        . ,prl:test_prop_InsertModel)
       ("prl:DeleteModel"        . ,prl:test_prop_DeleteModel)
       ("prl:UnionModel"         . ,prl:test_prop_UnionModel)
       ("prl:InsertInsert"       . ,prl:test_prop_InsertInsert)
       ("prl:InsertDelete"       . ,prl:test_prop_InsertDelete)
       ("prl:InsertUnion"        . ,prl:test_prop_InsertUnion)
       ("prl:DeleteInsert"       . ,prl:test_prop_DeleteInsert)
       ("prl:DeleteDelete"       . ,prl:test_prop_DeleteDelete)
       ("prl:DeleteUnion"        . ,prl:test_prop_DeleteUnion)
       ("prl:UnionDeleteInsert"  . ,prl:test_prop_UnionDeleteInsert)
       ("prl:UnionUnionIdem"     . ,prl:test_prop_UnionUnionIdem)
       ("prl:UnionUnionAssoc"    . ,prl:test_prop_UnionUnionAssoc)
       )
     )


   (checker-fn (dict-ref props search-key)))
  )
#lang racket/base

(module+ main
  (require racket/cmdline)
  (require rackcheck)
  (require racket/dict)
  (require "src/Sampling.rkt")

  (command-line
   #:program "rackcheck-bespoke"
   #:args info

   (define property (list-ref info 0))
   (define tests (string->number (list-ref info 1)))
   (define props
     `(
       ; Rackcheck properties
       ("InsertValid"        . ,sample_InsertValid)
       ("DeleteValid"        . ,sample_DeleteValid)
       ("UnionValid"         . ,sample_UnionValid)
       ("InsertPost"         . ,sample_InsertPost)
       ("DeletePost"         . ,sample_DeletePost)
       ("UnionPost"          . ,sample_UnionPost)
       ("InsertModel"        . ,sample_InsertModel)
       ("DeleteModel"        . ,sample_DeleteModel)
       ("UnionModel"         . ,sample_UnionModel)
       ("InsertInsert"       . ,sample_InsertInsert)
       ("InsertDelete"       . ,sample_InsertDelete)
       ("InsertUnion"        . ,sample_InsertUnion)
       ("DeleteInsert"       . ,sample_DeleteInsert)
       ("DeleteDelete"       . ,sample_DeleteDelete)
       ("DeleteUnion"        . ,sample_DeleteUnion)
       ("UnionDeleteInsert"  . ,sample_UnionDeleteInsert)
       ("UnionUnionIdem"     . ,sample_UnionUnionIdem)
       ("UnionUnionAssoc"    . ,sample_UnionUnionAssoc)
       )
     )


   (displayln (quick-sample (dict-ref props property) tests)))
  )
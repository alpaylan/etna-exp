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
       ("InsertPost"         . ,sample_InsertPost)
       ("DeletePost"         . ,sample_DeletePost)
       ("InsertModel"        . ,sample_InsertModel)
       ("DeleteModel"        . ,sample_DeleteModel)
       ("InsertInsert"       . ,sample_InsertInsert)
       ("InsertDelete"       . ,sample_InsertDelete)
       ("DeleteInsert"       . ,sample_DeleteInsert)
       ("DeleteDelete"       . ,sample_DeleteDelete)
       )
     )


   (displayln (quick-sample (dict-ref props property) tests)))
  )
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
       ("SinglePreserve"        . ,sample_SinglePreserve)
       ("MultiPreserve"        . ,sample_MultiPreserve)
       )
     )


   (displayln (quick-sample (dict-ref props property) tests)))
  )
#lang racket

(require (for-syntax racket/list))
(require (for-syntax racket/string))

(define-syntax (bc stx)
  (define input (second (syntax->datum stx)))
  (define input-tokens (string-split input))
  (define op (string->symbol (first input-tokens)))
  (displayln op)
  (define args (map string->number (rest input-tokens)))
  (displayln args)
 
  (datum->syntax stx `(exact->inexact (,op ,@args))))

(bc "/ 193 693")


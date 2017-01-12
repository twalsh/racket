#lang racket

(define ns (make-base-namespace))


(define (bc input)
 
  (define input-tokens (string-split input))
  (define op (string->symbol (first input-tokens)))
  (define args (map string->number (rest input-tokens)))
  (define expression `(exact->inexact (,op ,@args)))
  (eval expression ns))

(for ((input '("+ 1 2"
               "- 2 3"
               "* 3 4"
               "/ 4 5")))
  (printf "~a ~a~n" input (bc input)))

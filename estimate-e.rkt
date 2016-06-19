#lang racket

(define squares (* 8 8)) 

(define (estimate-e)
  (exact->inexact
   (/ squares (- squares (hash-count (for/hash ((_ (in-range squares)))
                                       (values (random squares) 1)))))))
  
(define trials 1000) 
  
(/ (for/sum ((_ (in-range trials))) (estimate-e)) trials)
  
#lang racket

(define squares (* 8 8)) 

(define (estimate-e)
  (define board (make-vector squares 0))
  
  (for ((i (in-range squares)))
    (let ((square (random squares)))
      (vector-set! board square (add1 (vector-ref board square)))))
  
  board
  
  (exact->inexact (/ squares (vector-count zero? board))))

(define trials 1000)

(/ (for/sum ((i (in-range trials)))
     (estimate-e))
   trials)

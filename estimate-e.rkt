#lang racket

(define squares (* 8 8)) 

(define (estimate-e)
  (let ((board (make-vector squares 0)))
    (for ((i (in-range squares)))
      (let ((square (random squares)))
        (vector-set! board square 1)))
    (exact->inexact (/ squares (vector-count zero? board)))))

(define trials 1000) 

(/ (for/sum ((i (in-range trials)))
     (estimate-e))
   trials)

#lang racket

; Knuth's TPK program in Racket.

(define (f x)
  (+ (sqrt (abs x)) (* 5 x x x)))

(define A (reverse 
           (for/list ((i (range 11)))
            (read))))

(for ((x A))
    (let ((y (f x)))
      (if (> y 400)
          (printf "~a too large~n" x)
          (printf "~a ~a~n" x y))))
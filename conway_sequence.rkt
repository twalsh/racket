#lang racket
;;; ~/m276.scm
;%I A004001 M0276
;%S A004001 1,1,2,2,3,4,4,4,5,6,7,7,8,8,8,8,9,10,11,12,12,13,14,14,15,15,15,16,
;%T A004001 16,16,16,16,17,18,19,20,21,21,22,23,24,24,25,26,26,27,27,27,28,29,29,
;%U A004001 30,30,30,31,31,31,31,32,32,32,32,32,32,33,34,35,36,37,38,38,39,40,41,42
;%N A004001 Hofstadter-Conway $10000 sequence: a(n) = a(a(n-1)) + a(n-a(n-1)) with a(1) = a(2) = 1.

(define (a n)
  (define v (make-vector (+ n 1)))
  (vector-copy! v 1 #(1 1))
  
  (for ((i (in-range 3 (add1 n))))
    (let ((an1 (vector-ref v (- i 1))))
      (vector-set! v i
                   (+ (vector-ref v an1) (vector-ref v (- i an1))))))
  v)

(define len 75)
(define seq (a len))

(printf "~a ~n" seq)

(for ((i (in-range 1 (add1 len))))
  (let ((n (vector-ref seq i)))
    (define ratio (exact->inexact (/ n i)))
    (printf "~a ~a ~a ~n" i n ratio)))


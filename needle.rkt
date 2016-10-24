#lang racket

(define (string->vector s) (list->vector (string->list s)))

(define seq1 (string->vector "GCATGCU"))
(define seq2 (string->vector "GATTACA"))

(define l1 (vector-length seq1))
(define l2 (vector-length seq2))

(for*/vector ((i (in-range (add1 l1)))
              (j (in-range (add1 l2))))
  (let (
        (s1 (if (= i 0) #\space (vector-ref seq1 (sub1 i))))
        (s2 (if (= j 0) #\space (vector-ref seq2 (sub1 j)))))
    (let ((score
           (cond ((eq? s1 #\space) (- j))
                 ((eq? s2 #\space) (- i))
                 ((eq? s1 s2) 1)
                 (else 0))))
      (list s1 s2 score))))

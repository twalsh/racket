#lang racket

(define (string->vector s) (list->vector (string->list s)))

(define A (string->vector "GCATGCU"))
(define B (string->vector "GATTACA"))

(define lA (add1 (vector-length A)))
(define lB (add1 (vector-length B)))

(define F (make-vector (* lA lB) 0))

(define d -1)
(define S 1)

(for ((i (in-range lA)))
  (vector-set! F i (* d i)))

(for ((j (in-range lB)))
  (vector-set! F (* j lA) (* d j)))

F

(define (cell m n)
  (vector-ref F (+ (* n lA) m)))

(define (cell! m n v)
  (vector-set! F (+ (* n lA) m) v))

(for* ((i (in-range 1 lA))
       (j (in-range 1 lB)))
  (let ((cA (vector-ref A (sub1 i)))
        (cB (vector-ref B (sub1 j))))
    (define S (if (eq? cA cB) 1 -1))
    (let ((match (+ (cell (sub1 i) (sub1 j)) S))
          (delete (+ (cell (sub1 i) j) d))
          (insert (+ (cell i (sub1 j)) d)))
      (cell! i j (max match insert delete)))))

F

(for ((j (in-range lB)))
  (for ((i (in-range lA)))
    (printf " ~a" (cell i j)))
  (newline))
 

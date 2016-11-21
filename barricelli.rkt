#lang racket

;(define size 16)

(define this-generation (vector 4 0 0 0 0 0 0 0 0 -2))

(define size (vector-length this-generation))

;(define this-generation
; (for/vector ((i (in-range size)))
;  (- size (random (* size 2)))))

(define next-generation (make-vector size 0))

this-generation

(for ((_ (in-range 4)))
  (for ((i (in-range size)))
    (define n (vector-ref this-generation i))
    (let reproduce ((j n))
      (unless (= j 0)
        (let ((k (abs (remainder (+ i j) size))))
          (printf "i: ~a j: ~a k:~a ~n" i j k)
          (unless (> (vector-ref next-generation k) 0)
            (printf "k ~a n: ~a ~n" k n)
            (vector-set! next-generation k n)
           
            (reproduce (vector-ref this-generation k)))))))

  (vector-copy! this-generation 0 next-generation)
  (vector-fill! next-generation 0)
  (displayln this-generation))

#lang racket

(define first-generation (make-vector 512 0))
(vector-set*! first-generation
              0 4
              9 -2)

(define size (vector-length first-generation))

(let loop ((g 0) (this-generation first-generation))
  (when (< g 5)
    (let ((next-generation (make-vector size 0)))
      (for ((i (in-range size)))
        (define n (vector-ref this-generation i))
        (let reproduce ((j n))
          (unless (= j 0)
            (let ((k (abs (remainder (+ i j) size))))
              
              (unless (> (vector-ref next-generation k) 0)
                (vector-set! next-generation k n))
              (let ((next-gene (vector-ref this-generation k)))
        ;        (printf "i ~a j ~a k ~a n ~a ng ~a~n" i j k n next-gene)
                (unless (= next-gene j)
                  (reproduce (vector-ref this-generation k))))))))
      (displayln (vector-take this-generation 10))
      (loop (add1 g) next-generation))))




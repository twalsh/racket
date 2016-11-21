#lang racket

(define first-generation (vector 4 0 0 0 0 3 0 0 0 -2))
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
                (printf "i ~a j ~a k ~a n ~a ng ~a~n" i j k n next-gene)
                (unless (= next-gene j)
                  (reproduce (vector-ref this-generation k))))))))
      (displayln this-generation)
      (loop (add1 g) next-generation))))

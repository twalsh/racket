#lang racket

(require math/matrix)
(require racket/set)

(define (initial-data max-base max-power)
  (let ((bases (range 1 (+ 1 max-base)))
        (powers (range 3 (+ 1 max-power))))
    (let ((pow (build-matrix (+ 1 max-base) (+ 1 max-power) expt)))
      (let ((table (set)))
        (for ((z bases) (r powers))
            (set-add table (matrix-ref pow z r)))
    (list bases powers table pow)))))
        

(define (beal max-base max-power)
  (let-values (((bases powers table pow) (initial-data max-base max-power)))
    (display bases)
    (display powers)
    (display table)))
  
(initial-data 2 4)
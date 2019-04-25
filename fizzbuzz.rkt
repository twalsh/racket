#lang racket


(define (fizz-buzz i)
  (define factor?
    (λ factors
      (for/and ((f factors))
        (integer? (/ i f)))))
  
  (define output
    (cond [(factor? 3) "Fizz"]
          [(factor? 5) "Buzz"]
          [(factor? 3 5) "FuzzBuzz"]
          [else i]))
  (println output))
 
(for-each fizz-buzz (range 101))
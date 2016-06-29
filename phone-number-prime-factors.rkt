#lang racket

(require "prime.rkt")
(require "utils.rkt")

(define lines (text-csv-read "google.csv"))

;(send lines get-data)

;(send lines get-column-index "Phone 1 - Value")

(define numbers
  (send lines get-column "Phone 1 - Value"))

(define (number-clean s)
  (define numbers
    (if (string-contains? s ":::")
        (string-split s " ::: ")
        (list s)))
  (map (lambda (s)
         (define char-list (string->list s))
         (list->string
          (filter (lambda (c)
                    (and (char<=? #\0 c) (char<=? c #\9)))
                  char-list))) numbers))

(define clean-numbers
  (map string->number
       (filter (lambda (n)
                 (not (string=? n "")))
               (flatten
                (map number-clean numbers)))))

(define (factors n)
  (let loop ((f 2) (factor-list '()))
    (if (> f (/ n 2))
        factor-list
        (if (= (remainder n f) 0)
            (loop (add1 f) (cons f factor-list))
            (loop (add1 f) factor-list)))))

(define prime-table (primes-below (inexact->exact (floor (sqrt (apply max clean-numbers))))))

;def trial_division(n):
;    """Return a list of the prime factors for a natural number."""
;    if n < 2:
;        return []
;    prime_factors = []
;    for p in prime_sieve(int(n**0.5)):
;        if p*p > n: break
;        while n % p == 0:
;            prime_factors.append(p)
;            n //= p
;    if n > 1:
;        prime_factors.append(n)
;    return prime_factors

(define (prime-factors n)
  ; Convert set to list
  (set->list
   ; Remove repeated factors
   (list->set
    (if (< n 2)
        '()
        (let loop ((pt prime-table) (pl '()) (n1 n))
          (define p (first pt))
          ;(printf "L1: ~a ~a ~a~n" n1 p pl)
          (if (> (* p p) n1)
              (if (> n1 1)
                  (cons n1 pl)
                  pl)
              (let loop2 ((n2 n1) (pl2 pl))
                ;(printf "L2: ~a ~a~n" n2 pl2)
                (if (= (remainder n2 p) 0)
                    (loop2 (floor (/ n2 p)) (cons p pl2))
                    (loop (rest pt) pl2 n2)))))))))

(time   
   (for-each
    (lambda (n)
      (printf "~a ~a~n" n (prime-factors n)))
    clean-numbers)
   )

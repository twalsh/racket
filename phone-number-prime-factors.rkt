#lang racket

(require "prime.rkt")
(require "utils.rkt")

(define lines (text-csv-read "google.csv"))

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

(define prime-table (primes-below (inexact->exact (floor (sqrt (apply max clean-numbers))))))

(define (find-factors-in-prime-series n pt)
  (if (empty? pt)
      '()
      (let ((p (first pt)))
        (if (> (* p p) n)
            (if (> n 1)
                (list n)
                '())
            (reduce-factor n p (rest pt))))))

(define (reduce-factor n p ps)
  (if (= (remainder n p) 0)
      (cons p (reduce-factor (floor (/ n p)) p ps))
      (find-factors-in-prime-series n ps)))

(define (prime-factors n)
  (define factors
    (set->list
     (list->set
      (if (< n 2)
          '()
          (find-factors-in-prime-series n prime-table)))))
  (sort factors <=))

(time   
 (for-each
  (lambda (n)
    (printf "~a ~a~n" n (prime-factors n)))
  clean-numbers)
 )



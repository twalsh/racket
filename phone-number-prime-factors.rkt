#lang racket

(require plot)

(require "prime.rkt")
(require "utils.rkt")

(define lines (text-csv-read "google.csv"))

(define numbers
  (send lines get-column "Phone 1 - Value"))

; Remove characters from a string that aren't digits 1-9 and return the
; digits as a list
(define (remove-nondigits s)
  (define char-list (string->list s))
  (filter (lambda (c) (and (char<=? #\1 c) (char<=? c #\9)))
          char-list))

(define (number-clean s)
  ; Split number field if it contains 2 numbers separated by :::
  (define numbers
    (if (string-contains? s ":::")
        (string-split s " ::: ")
        (list s)))
  (map remove-nondigits numbers))

; Cleaned-up numbers as lists of digits
(define clean-number-lists
  (append*
   (filter (lambda (n)
             (not (empty? (flatten n))))
           (map number-clean numbers))))

(define max-number-length (apply min (map length clean-number-lists)))

;max-number-length

(define clean-numbers
  (map
   (lambda (digit-list)
     (string->number
      (list->string
       (take-right digit-list max-number-length))))
   clean-number-lists))

;clean-numbers

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

(define factors   
 (map
  (lambda (n)
    (list n (prime-factors n)))
  clean-numbers)
 )

;factors

(define freq (make-hash))

(define number-of-factors (map (lambda (f) (length (second f))) factors))

number-of-factors

(for-each
 (lambda (nf)
   (hash-update! freq nf add1 0))
 number-of-factors)

(define freq-vector-list (map flatten (hash->list freq)))

(plot-new-window? #t)
(plot (discrete-histogram freq-vector-list))

 



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

(map factors clean-numbers)


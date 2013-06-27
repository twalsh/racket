#lang racket
(define cipher-text "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJHUBT LZA ULBAYVU")

(define cipher-list (string->list cipher-text))

(define (rotate-letter letter n)
  (if (equal? letter #\space)
      letter
      (let ((ascii+ (+ n (char->integer letter))))
        (if
         (> ascii+ 90)  (integer->char (- ascii+ 26))
         (integer->char ascii+)
         ))))

(for ((i (in-range 1 26)))
  (let ((rotation (map (curryr rotate-letter i) cipher-list)))
    (printf "~a ~a~n" i (list->string rotation))))
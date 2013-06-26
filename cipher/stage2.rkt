#lang racket
(define cipher-text "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ

KPJABT HYJHUBT LZA ULBAYVU")

(define cipher-list (string->list cipher-text))

(define (rotate-letter letter n)
  (let ((ascii+ (+ n (char->integer letter))))
    (if (> ascii+ 90)
        (integer->char (- ascii+ 26))
        (integer->char ascii+))))

(rotate-letter #\A 1)
(rotate-letter #\Y 1)
(rotate-letter #\Z 1)
;(let loop ((rotation cipher-list))
 ;(unless (empty? rotation)
    
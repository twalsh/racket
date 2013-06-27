#lang racket
(define cipher-text "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJHUBT LZA ULBAYVU")

(define cipher-list (string->list cipher-text))

(for ((i (in-range 1 26)))
  (let ((rotation (map (lambda (letter)
                         (if (eq? letter #\space)
                             letter
                             (let ((ascii+ (+ i (char->integer letter))))
                               (if
                                (> ascii+ 90)  (integer->char (- ascii+ 26))
                                (integer->char ascii+)
                                ))))
                       cipher-list)))
    (printf "~a ~a~n" i (list->string rotation))))
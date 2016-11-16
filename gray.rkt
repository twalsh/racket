#lang racket

(define (gray-code n)
  (define (make-layer n)
    (if (= n 1)
        (list '(0 1))
        (let ((ld (make-layer (sub1 n))))
          (define mirror
            (map (lambda (l) (flatten (list l (reverse l))))
                 ld))
          (define next-level (flatten (list (make-list n 0) (make-list n 1))))
          (printf "~a~n" mirror)
          (printf "~a~n" next-level)
          (cons next-level mirror))))
  (define layers (make-layer n))
  layers)
        
(gray-code 1)
(gray-code 2)
(gray-code 3)



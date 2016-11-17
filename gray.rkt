#lang racket

(define (gray-code n)
  (define (make-layer n)
    (if (= n 1)
        (list '(#\0 #\1))
        (let ((ld (make-layer (sub1 n))))
          (define mirror
            (map (lambda (l) (flatten (list l (reverse l))))
                 ld))
          (define nlw (expt 2 (sub1 n)))
          (define next-level (flatten (list (make-list nlw #\0) (make-list nlw #\1))))
          (cons next-level mirror))))
  (define layers (make-layer n))
  (reverse 
   (let loop ((layers layers) (codes '()))
     (if (empty? (first layers))
         codes
         (let ((next-code
                (list->string (map car layers))))
           (loop (map rest layers) (cons next-code codes))
           )))))
        
(gray-code 1)
(gray-code 2)
(gray-code 3)



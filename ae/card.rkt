#lang racket
(provide read-lines)

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define store (make-vector 10 0))
(define ingress (make-vector 2 0))
(define egress 0)

(define (read-card line)
  (let ((instr (string-ref line 0)))
    (cond ((eq? instr #\N)
           (match-let (((list col val) (string-split (substring line 1) " ")))
               (display col)(newline)(display val)(newline)
               (vector-set! store (string->number col) (string->number val))))
          ((eq? instr #\+)
           (set! egress (apply + ingress)))
          ((eq? instr #\-)
           (set! egress (apply - ingress)))
          ((eq? instr #\*)
           (set! egress (apply * ingress)))
          ((eq? instr #\/)
           (set! egress (apply / ingress))))))

(define cards '(
                "N001 1"
                "N002 2"))

(map read-card cards)

store
ingress
egress

          
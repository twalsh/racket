#lang racket
(provide read-lines)

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define store (make-vector 10 0))
(define ingress '())
(define egress (lambda (a b) '()))

(define (read-card line)
  (let ((instr (string-ref line 0)))
    (cond ((eq? instr #\N)
           (match-let (((list col val) (string-split (substring line 1) " ")))
               (display col)(newline)(display val)(newline)
               (vector-set! store (string->number col) (string->number val))))
          ((eq? instr #\+)
           (set! egress +))
          ((eq? instr #\-)
           (set! egress -))
          ((eq? instr #\*)
           (set! egress *))
          ((eq? instr #\/)
           (set! egress /))
          ((eq? instr #\L)
           (
          )))

(define cards '(
                "N001 1"
                "N002 2"))

(map read-card cards)

store
ingress
egress

          
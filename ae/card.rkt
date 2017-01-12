#lang racket
(provide read-lines)

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
          (reverse lines)
          (loop (cons line lines))))))

(define store (make-vector 20 0))
(define (put col val)
  (vector-set! store (string->number col) (string->number val)))
(define (get col)
  (vector-ref store (string->number col)))

(define (divn)
  (printf "DIV ~a ~a~n" ingress1 ingress2)
  (define-values (q r) (quotient/remainder ingress1 ingress2))
  
  (set! egress1 q)
  (set! egress2 r))

(define ingress1 empty)
(define ingress2 empty)

(define (mult)
  (set! egress2 (* ingress1 ingress2))
  (set! egress1 0))

(define egress1 empty)
(define egress2 empty)

(define (step-down egress shift)
  (set! egress1 (/ egress1 (expt 10 shift)))
  (set! egress2 (/ egress2 (expt 10 shift))))

(define (step-up ingress shift)
  (map (lambda (n) (* n (expt 10 shift))) ingress))

(define op 'no-op)

(define (read-card lines)
  (for ((line lines))
    (cond ((not (empty? ingress2))
           (op)
           (set! ingress1 empty)
           (set! ingress2 empty)
           )
          (else
           (let ((instr (string-ref line 0)))
             (match instr
               (#\N (match-let (((list col val) (string-split (substring line 1) " ")))
                      (put col val)
                      ))
               ((or #\+ #\- #\* #\/)
                (set! op (match instr
                           (#\+ +)
                           (#\- -)
                           (#\* mult)
                           (#\/ divn))))
               ((or #\L #\Z)
                (let ((col (substring line 1)))
                  (when (eq? instr #\Z) (put col 0))
                  (define value (get col))
                  (if (empty? ingress1)
                      (set! ingress1 value)
                      (set! ingress2 value))))
               (#\S (let ((col (string->number (substring line 1 4))) (prime (string-ref line 4)))
                      (if (eq? prime #\')
                          (vector-set! store col egress1)
                          (vector-set! store col egress2))))
               ((or #\> #\<)
                (let ((shift (string->number (substring line 1))))
                  (if (eq? instr #\>)
                      (step-down shift)
                      (step-up shift))))
               (#\H (printf "~s~n" line))
               (#\P (printf "EGRESS ~s ~s~n" egress1 egress2))
               (#\space 'else)
               ))))
    (printf "ING ~s ~s OP ~v EG ~s ~s STORE ~s~n" ingress1 ingress2 op egress1 egress2 store)
    ))

(define cards '(
                "N001 10"
                "N002 5"
                "/"
                "L001"
                "L002"
                "P"
                "S003'"
                "S004 "
                "P"
                " remark"
                "N010 4"
                "N011 25"
                "N012 280"
                "*"
                "L010"
                "L011"
                "P"                
                ;                ">6"
                ;                "S013 "
                ;                "/"
                ;                "L013"
                ;                "<6"
                ;                "L012"
                ;                "S014'"
                ;                "P"
                ;                "H STOP"
                ))

(read-card cards)

store


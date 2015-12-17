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

(define (divn ingress)
  (match-let (((list x y) (reverse ingress)))
    (let-values (((q r) (quotient/remainder x y)))
      (list q r))))

(define (mult ingress)
  (list 0 (apply * ingress)))

(define (step-down egress shift)
  (map (lambda (n) (/ n (expt 10 shift))) egress))

(define (step-up ingress shift)
  (map (lambda (n) (* n (expt 10 shift))) ingress))

(define (read-card lines)
  (let loop ((cards lines) (ingress '()) (op (lambda (a b) '())) (egress '()))
    (if (= (length ingress) 2)
        (loop cards '() op (op ingress))
        (if (not (empty? cards))
            (let ((line (car cards)))
              (let ((instr (string-ref line 0)))
                (match instr
                  (#\N (match-let (((list col val) (string-split (substring line 1) " ")))
                         (put col val)
                         (loop (cdr cards) ingress op egress)
                         ))
                  (#\+ (loop (cdr cards) ingress + egress))
                  (#\- (loop (cdr cards) ingress - egress))
                  (#\* (loop (cdr cards) ingress mult egress))
                  (#\/ (loop (cdr cards) ingress divn egress))
                  (#\L (let ((col (substring line 1)))
                        (loop (cdr cards) (cons (get col) ingress) op egress)))
                  (#\Z (let ((col (substring line 1)))
                         (put col 0)
                         (loop (cdr cards) (cons (get col) ingress) op egress)))
                  (#\S (let ((col (string->number (substring line 1 4))) (prime (string-ref line 4)))
                         (if (eq? prime #\')
                             (vector-set! store col (car egress))
                             (vector-set! store col (cadr egress)))
                         (loop (cdr cards) ingress op egress)))
                  (#\> (let ((shift (string->number (substring line 1))))
                         (loop (cdr cards) ingress op (step-down egress shift))))
                  (#\< (let ((shift (string->number (substring line 1))))
                         (loop (cdr cards) (step-up ingress shift) op egress)))
                  (#\space (loop (cdr cards) ingress op egress))
                  (#\H (printf "~s~n" line))
                  (#\P (begin (printf "~s~n" egress)
                              (loop (cdr cards) ingress op egress)))
                  )))
            (printf "I ~s O ~s E ~s~n" ingress op egress)
            ))))

(define cards (list
                "N001 10000"
                "N002 28"
                "/"
                "L001"
                "L002"
                "S003'"
                "S004 "
                " remark"
                "N010 4000000000"
                "N011 2500000"
                "N012 28000000"
                "*"
                "L010"
                "L011"
                ">6"
                "S013 "
                "/"
                "L013"
                "<6"
                "L012"
                "S014'"
                "P"
                "H STOP"
                ))

(read-card cards)

store

          
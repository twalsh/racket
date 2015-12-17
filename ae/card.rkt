#lang racket
(provide read-lines)

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define store (make-vector 10 0))
(define (put col val)
  (vector-set! store (string->number col) (string->number val)))
(define (get col)
  (vector-ref store (string->number col)))

(define egress 0)
(define egressp 0)

(define (divn ingress)
  (match-let (((list x y) (reverse ingress)))
    (let-values (((q r) (quotient/remainder x y)))
      (list q r))))
    
(define (read-card lines)
  (let loop ((cards lines) (ingress '()) (op (lambda (a b) '())) (egress '()))
    (if (= (length ingress) 2)
        (let ((result (op ingress)))
          (loop cards '() op result))
        (if (not (empty? cards))
            (let ((line (car cards)))
              (let ((instr (string-ref line 0)))
                (cond ((eq? instr #\N)
                       (match-let (((list col val) (string-split (substring line 1) " ")))
                         (display col)(newline)(display val)(newline)
                         (put col val)
                         (loop (cdr cards) ingress op egress)
                         ))
                      ((eq? instr #\+)
                       (loop (cdr cards) ingress + egress))
                      ((eq? instr #\-)
                       (loop (cdr cards) ingress - egress))
                      ((eq? instr #\*)
                       (loop (cdr cards) ingress * egress))
                      ((eq? instr #\/)
                       (loop (cdr cards) ingress divn egress))
                      ((eq? instr #\L)
                       (let ((col (substring line 1)))
                         (printf "L ~s~n" col)
                         (loop (cdr cards) (cons (get col) ingress) op egress)))
                      ((eq? instr #\S)
                       (let ((col (string->number (substring line 1 4))) (prime (string-ref line 4)))
                         (printf "S ~s ~s~n" col prime)
                         (if (eq? prime #\')
                             (vector-set! store col (car egress))
                             (vector-set! store col (cadr egress)))
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
                ))

cards
(read-card cards)

store
;ingress
;egress

          
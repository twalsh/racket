#lang racket
(require rackunit)

(define nice-s (list "ugknbfddgicrmopn" "aaa"))

(define naughty-s (vector "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"))

(define rules 
  (vector
   (cons "At least 3 vowels"
   (lambda (s)
     (>= 3 (length (regexp-match* #px"[aeiou]" s)))))
   (cons "At least 1 doubled letter" 
   (lambda (s)
     (not (empty? (regexp-match* #px"([a-z])\\1" s)))))
   (cons "No ab,cd,pq or xy" 
   (lambda ( s)
     (not (regexp-match #px"(ab|cd|pq|xy)" s))))))

(define (rule i)
  (cdr (vector-ref rules (- i 1))))

(define (comb s)
  (and ((rule 1) s)
       ((rule 2) s)
       ((rule 3) s)))

(for ((s nice-s))
  (check-equal? (comb s) #t s))

(define (n i)
  (vector-ref naughty-s i ))

(let ((rule (vector-ref rules 0)))
  (printf "~s~n" (car rule))
  (check-equal? ((cdr rule) (n 0)) #t (n 0))
  (check-equal? ((cdr rule) (n 1)) #t (n 1))
  (check-equal? ((cdr rule) (n 2)) #f (n 2)))

(let ((rule (vector-ref rules 1)))
  (printf "~s~n" (car rule))
  (check-equal? ((cdr rule) (n 0)) #f (n 0))
  (check-equal? ((cdr rule) (n 1)) #t (n 1))
  (check-equal? ((cdr rule) (n 2)) #t (n 2)))

(let ((rule (vector-ref rules 2)))
  (printf "~s~n" (car rule))
  (check-equal? ((cdr rule) (n 0)) #t (n 0))
  (check-equal? ((cdr rule) (n 1)) #f (n 1))
  (check-equal? ((cdr rule) (n 2)) #t (n 2)))

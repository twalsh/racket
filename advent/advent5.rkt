#lang racket
(require rackunit)

(define nice-s (list "ugknbfddgicrmopn" "aaa"))

(define naughty-s (list "jchzalrnumimnmhp" "haegwjzuvuyypxyu" "dvszwmarrgswjxmb"))

; At least 3 vowels
(define (rule1 s)
  (>= 3 (length (regexp-match* #rx"[aeiou]" s))))

; At least 1 doubled letter
(define (rule2 s)
  (not (empty? (regexp-match* #px"([a-z])\\1" s))))
s
; No ab,cd,pq or xy
(define (rule3 s)
  (not (regexp-match #rx"(ab|cd|pq|xy)" s)))

(map rule1 nice-s)
(map rule1 naughty-s)

(map rule2 nice-s)
(map rule2 naughty-s)

(map rule3 nice-s)
(map rule3 naughty-s)

(check-equal? (rule1 (car nice-s)) #t "rule 1") 

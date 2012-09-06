#lang racket
(require racket/set)

(define fasta "ACCTGAGACTGACNTGACTTGACTGA")

(define nuc (set #\A #\C #\G #\T))

(for/fold ((i 0))
  ((c (in-string fasta)) #:unless (char=? c #\N))
  (let ((diff (list->string (set->list (set-remove nuc c)))))
    (printf "~a ~a ~a\n" c (+ i 1) diff))
  (+ i 1)
  )
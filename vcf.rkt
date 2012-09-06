#lang racket
(require racket/set)

(define fasta "ACCTGAGACTGACNTGACTTGACTGA")

(define nuc (set #\A #\C #\G #\T))

(printf "~a\n" (string-join '("#CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO") "\t"))

(for/fold ((i 0))
  ((c (in-string fasta)) #:unless (char=? c #\N))
  (let ((alt (list->string (set->list (set-remove nuc c)))))
    (printf "~a\n" ;"~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\n" 
            (string-join (list "chr" (number->string (+ i 1)) "." (string c) "alt" "100" "PASS" "DP=100") "\t")))
  (+ i 1)
  )
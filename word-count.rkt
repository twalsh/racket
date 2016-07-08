#lang racket

(define in (open-input-file "ulysses.txt"))

(define word-count
  (let ((wc (make-hash)))
    (hash->list
     (let next-line ((line (read-line in)))
       (if (eof-object? line)
           wc
           (let ((words (map string-downcase (string-split line #px"[\\s.,?;:-]+"))))
             (for-each
              (lambda (word) (hash-update! wc word add1 0))
              words)
             (next-line (read-line in))))))))

(take (sort word-count > #:key cdr) 20)

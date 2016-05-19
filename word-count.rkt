#lang racket

(define in (open-input-file "ulysses.txt"))

(define word-count
  (hash->list
   (let loop ((wc (make-immutable-hash)))
     (let ((line (read-line in)))
       (if (eof-object? line)
           wc
           (let add-word ((nwc wc) (words (string-split line #px"[\\s.,?;:-]+")))
             (if (empty? words)
                 (loop nwc)
                 (let ((next-word (string-downcase (first words))))
                   (add-word (hash-update nwc next-word add1 0) (rest words))))))))))

(take (sort word-count > #:key cdr) 20)

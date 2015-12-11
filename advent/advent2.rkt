#lang racket

(define (get-ribbon dimensions)
  (+ (* 2 (apply + (take (sort dimensions <) 2)))
     (apply * dimensions)))

(define (get-paper dimensions)
  (match-let (((list l w h) dimensions))
    (+ (* 2
          (+
           (* l w)
           (* w h)
           (* h l)))
       ; Extra paper
       (apply * (take (sort dimensions <) 2)))))

(define (read-input in)
  (let loop ((line (read-line in)) (total-paper 0) (total-ribbon 0))
    (if (eof-object? line)
        (cons total-paper total-ribbon)
        (let ((dimensions (map string->number (string-split line "x"))))
          (let ((paper (get-paper dimensions))
                (ribbon (get-ribbon dimensions)))
            (loop (read-line in) (+ total-paper paper) (+ total-ribbon ribbon))
            )))))


(call-with-input-file "input2.txt" read-input)


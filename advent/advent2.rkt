#lang racket

(define (wrap-present line)
  (let ((dimensions (map string->number (string-split line "x"))))
    (match-let (((list l w h) dimensions))
    (+ (* 2
          (+
           (* l w)
           (* w h)
           (* h l)))
       ; Extra paper
       (apply * (take (sort dimensions <) 2))))))

(define (read-input in)
  (let loop ((line (read-line in)) (total-area 0))
    (if (eof-object? line)
        total-area
        (let ((area (wrap-present line)))
          (loop (read-line in) (+ total-area (wrap-present line))
                )))))


(call-with-input-file "input2.txt" read-input)


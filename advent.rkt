#lang racket

(define file "/home/tom/advent/adventure.text")

(define sections (make-hash))

(define (read-section)
  (let ((header (read-line)))
    (if (string=? header "0")
        '()
        (let while ((lines '()))
          (let ((line (read-line)))
            (if (string=? line "-1")
                (list header (reverse lines))
                (while (cons line lines))))))))

(define main
  (with-input-from-file file
    (lambda () 
      (do ([section (read-section) (read-section)])
        ((null? section))
        (let ((header (car section)) (data (cadr section)))
          (hash-set! sections header data))))))

(define section-reader (make-vector 15))

(vector-set! section-reader 1
  (let ((long-desc (make-vector 185)))
    (vector-set! long-desc 0 "Long description")
    (let loop ((data (hash-ref sections "1")))
      (match-let ([(list loc text) (regexp-split #rx"\t" (car data))])
        (vector-set! long-desc (string->number loc) text)
        (if (null? (cdr data))
            long-desc
            (loop (cdr data))
            
            )))))
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

(define long-desc (make-vector 184))

(define (get-text line)
  (display line)
  (cadr (regexp-split #rx"\t" line)))

(define process-1
  (let loop ((data (hash-ref sections "1")))
             (let ((text (get-text (car data))))
    (display text)
    (loop (cdr data))
               
               )))
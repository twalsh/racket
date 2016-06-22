#lang racket

(provide read-lines read-input text-csv-read)

(define (read-input file)
  (read-lines (open-input-file file)))

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
          (reverse lines)
          (loop (cons line lines))))))

(define text-csv%
  

(define (text-csv-read file)  
  (let ((lines (read-input file)))
    ; Split lines into fields
    (let ((line-fields (map (lambda (line) (string-split line ",")) lines)))
      ; Make vector of field names from first line
      (let ((field-names (list->vector (first line-fields))))
        ; Create hash mapping field names to numbers
        (let ((dict (for/hash ((i (in-range (vector-length field-names))))
                      (values (vector-ref field-names i) i))))
          (list dict (rest line-fields)))))))

(define (text-csv-field-ref text-csv field-name)
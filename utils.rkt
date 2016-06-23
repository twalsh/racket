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
  (class object%
    (init header data)
    (define _header header)
    (define _data data)
    (super-new)
    (define/public (get-header) _header)
    (define/public (get-data) _data)
    (define/public (get-column-index column-name)
      (hash-ref _header column-name))
    (define/public (get-column column-name)
      (define column-index (hash-ref _header column-name))
      (map (lambda (row) (vector-ref row column-index)) _data))
    ))

(define (text-csv-read file)  
  (let ((lines (read-input file)))
    ; Split lines into fields
    (let ((line-fields (map (lambda (line) (list->vector (string-split line ","))) lines)))
      ; Make vector of field names from first line
      (let ((field-names (first line-fields)))
        ; Create hash mapping field names to numbers
        (let ((dict (for/hash ((i (in-range (vector-length field-names))))
                      (values (vector-ref field-names i) i))))
          (new text-csv% (header dict) (data (rest line-fields))))))))

;(define (text-csv-field-ref text-csv field-name)
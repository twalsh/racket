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
    (define .header header)
    (define .data data)
    (define header-length (hash-count .header))
    (super-new)
    (define/public (get-header) .header)
    (define/public (get-data) .data)
    (define/public (get-header-length) header-length)
    (define/public (get-column-index column-name)
      (hash-ref .header column-name))
    (define/public (get-column column-name)
      (define column-index (hash-ref .header column-name))
      (map (lambda (row) (vector-ref row column-index)) .data))
    ))

(define (text-csv-read file)  
  (define lines (read-input file))
  ; Split each line into a vector of cells
  (define row-vectors (map (lambda (line) (list->vector (string-split line ","))) lines))
  ; Make vector of column names from first line
  (define columns (first row-vectors))
  (define number-of-columns (vector-length columns))
  ; Remove lines that don't have the expected number of columns
  (define data-rows
    (filter (lambda (field-vector)
              (= (vector-length field-vector) number-of-columns))
            (rest row-vectors)))
  ; Create hash mapping field names to numbers
  (define dict (for/hash ((i (in-range (vector-length columns))))
                 (values (vector-ref columns i) i)))
  
  (new text-csv% (header dict) (data data-rows)))

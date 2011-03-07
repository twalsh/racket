#lang racket

(require mzlib/defmacro)
(require racket/serialize)
(require srfi/1)

(define (make-cd title artist rating ripped)
  (make-hash (list (cons 'title title) (cons 'artist artist) (cons 'rating rating)
                   (cons 'ripped ripped))))

(define *db* '())

(define (add-record cd)
  (set! *db* (cons cd *db*)))

(define (dump-db)
  (for-each (lambda (cd) (printf "~a / ~a / ~a ~n" 
                                 (hash-ref cd 'title)
                                 (hash-ref cd 'artist)
                                 (hash-ref cd 'rating)
                                 )) *db*))

(define cd1 (make-cd "Undertow" "Warpaint" 10 #t))
(define cd2 (make-cd "Metal Heart" "Cat Power" 9 #t))
(define cd3 (make-cd "Sugar" "Ladytron" 8 #t))
(for-each add-record (list cd1 cd2 cd3))
(add-record (make-cd "Bees" "Warpaint" 9 #t))

(define (prompt-read prompt-text)
  (display prompt-text)
  (read))

(define (prompt-for-cd)
  (make-cd
   (prompt-read "title")
        (prompt-read "artist")
        (prompt-read "rating")
        (prompt-read "ripped")))

(define (save-db filename)
  (with-output-to-file filename
    (lambda () (write (serialize *db*)))))

(define (load-db filename)
  (with-input-from-file filename
    (lambda () (set! *db* (deserialize (read))))))

(define (select selector-fn)
  (filter selector-fn *db*))

(define (where #:title (title '()) #:artist (artist '()) 
            #:rating (rating '()) #:ripped (ripped '()))
  (lambda (cd)
    (and
     (if (not (null? title)) (string=? (hash-ref cd 'title) title) #t)
     (if (not (null? artist)) (string=? (hash-ref cd 'artist) artist) #t)
     (if (not (null? rating)) (string=? (hash-ref cd 'rating) rating) #t)
     (if (not (null? ripped)) (eq? (hash-ref cd 'ripped) ripped) #t))))
     


(define (make-comparisons-expr field value)
    (list 'string=? (list 'hash-ref 'cd field) value))

(define (make-comparisons-list field-value-pairs)
  (let loop ((fields field-value-pairs) (comparisons '()))
    (if (empty? fields)
        comparisons
        (loop (cddr fields) 
              (cons (make-comparisons-expr (car fields) (cadr fields)) comparisons)))))

(defmacro where2 clauses
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses)))) 
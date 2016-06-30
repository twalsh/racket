#lang racket

(require "slurp.rkt")

(define lines (slurp-file "C:/users/tom/OneDrive/Documents/Furniture.csv"))

(define (process-item item)
  (define description (first item))
  (define dimensions (map (lambda (line)
                            (string->number
                             (substring line 2))) (rest item)))
  (values description dimensions))


(define total-volume
(let loop ((remaining-lines lines) (total-volume 0))
  (if (not (empty? remaining-lines))
    (let ((next-item (map string-trim (take remaining-lines 4))))
      (define-values (description dimensions) (process-item next-item))
      (define W (first dimensions))
      (define H (second dimensions))
      (define D (third dimensions))
      (define volume (* W H D))
      (printf "~a,~a,~a,~a,~a,1~n" description W H D volume)
      (loop (drop remaining-lines 5) (+ total-volume volume)))
    total-volume)))

total-volume
(define total-volume-cubic-feet (/ total-volume (* 12 12 12)))
total-volume-cubic-feet
(define total-volume-cubic-metres (/ (* total-volume (expt 2.54 3)) 1000000))
total-volume-cubic-metres

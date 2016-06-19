#lang racket

(define (picks)
  (let loop ((n 0) (sum 0))
    (if (> sum 1)
        n
        (loop (add1 n) (+ sum (random))))))

(define (estimate-e ntrials) 
  (let ((trials (for/list ((_ (in-range ntrials)))
                  (picks))))
    (exact->inexact (/ (apply + trials) ntrials))))

(estimate-e 100000)

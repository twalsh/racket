#lang racket

(define (picks)
  (let loop ((n 0) (sum 0))
    (if (> sum 1)
        n
        (loop (add1 n) (+ sum (random))))))

(define (estimate-e ntrials) 
  (let ((sum-of-trials (for/sum ((_ (in-range ntrials)))
                         (picks))))
    (exact->inexact (/ sum-of-trials ntrials))))

(estimate-e 100000)

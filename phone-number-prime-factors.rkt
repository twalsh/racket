#lang racket

(require "prime.rkt")
(require "utils.rkt")

(define lines (text-csv-read "google.csv"))

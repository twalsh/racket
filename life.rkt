#lang racket

(define grid-size 9)

(define grid-length (* grid-size grid-size))

(define (border-cell? p)
  (or (< p grid-size)
      (>= p (- grid-length grid-size))
      (= (remainder p grid-size) 0)
      (= (remainder (add1 p) grid-size) 0)))

(define grid
  (list->vector
   (for/list ((p (in-range grid-length)))
     (if (border-cell? p)
         0
         1))))

(define cell-indexes
  (for/list ((p (in-range grid-length))
             #:unless (border-cell? p))
    p))

(define (show-grid)
  (newline)
  (for ((i (in-range grid-length)))
    (when (= (remainder i grid-size) 0)
      (newline))
    (define char
      (let ((v (vector-ref grid i)))
        (cond ((= v 1) #\space)
              ((= v 2) #\*)
              (else #\|))))
    (printf "~a" char)))

(define (cell-set! coor value)
  (define p
    (+ (* grid-size (first coor)) (second coor)))
  (vector-set! grid p value))

(define (grid-set! cells)
  (when (not (empty? cells))
    (let ((cell (take cells 2)))
      (cell-set! cell 2)
      (grid-set! (drop cells 2)))))

(define (cell-neighbours p)
  (define grid-score
    (for/sum ((offset
               (list (- 0 grid-size 1)
                     (- 0 grid-size)
                     (- 0 grid-size -1)
                     -1
                     1
                     (+ grid-size -1)
                     grid-size
                     (+ grid-size 1))))
      (define np (+ p offset))
      (vector-ref grid np)))
  (- grid-score 8))

(grid-set! '(4 3
               4 4
               4 5
               5 2
               5 3
               5 4
               ))

(show-grid)

(define (grid-update)
  (for/vector ((p cell-indexes))
    (define cell-score (cell-neighbours p))
    (define cell-state
      (if (= (vector-ref grid p) 2)
          'LIVE
          'DIE))
    (define fate
      (if (eq? cell-state 'LIVE)
          (if (or (< cell-score 2) (> cell-score 3))
              'DIE
              'LIVE)
          (if (= cell-score 3)
              'LIVE
              'DIE)))
    (if (eq? fate 'LIVE)
        2
        1)))

(for ((g (in-range 3)))
  (define updates (grid-update))
  (for ((i (in-range (length cell-indexes))))
    (define p (list-ref cell-indexes i))
    (define s (vector-ref updates i))
    (vector-set! grid p s))
  (show-grid))

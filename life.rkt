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
    
(define (cell-pos coor)
  (+ (* grid-size (first coor)) (second coor)))

(define (cell-ref coor)
  (define pos (cell-pos coor))
  (vector-ref grid pos))

(define (show-grid)
  (newline)
  (for ((i (in-range grid-size)))
    (for ((j (in-range grid-size)))
      (define char
        (let ((v (cell-ref (list i j))))
          (cond ((= v 1) #\space)
                ((= v 2) #\.)
                (else #\|))))
      (printf "~a" char)
      )
    (newline)))

(define (cell-set! coor value)
  (vector-set! grid (cell-pos coor) value))

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
               4 5))

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
    ;(printf "~a ~a ~a ~a~n" p cell-state cell-score fate)
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

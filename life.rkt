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
      (printf "~a" (cell-ref (list i j)))
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
  (- grid-score 6))

(grid-set! '(4 3
               4 4
               4 5))

;(for ((i (in-range 10)))
(show-grid)

;(cell-neighbours 2 2)
cell-indexes
(length (map (lambda (p) (vector-ref grid p)) cell-indexes))

; 6 - 0 neighours
; 7 - 1 neighbours
; 8 - 2 neighbours
; 9 - 3 neighbours
; 10 - 4 neighbours

(for ((p cell-indexes))
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
  (printf "~a ~a ~a ~a~n" p cell-state cell-score fate))
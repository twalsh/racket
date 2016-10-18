#lang racket

(define grid-size 7)

(define grid-length (* grid-size grid-size))

(define grid
  (list->vector
   (for/list ((p (in-range grid-length)))
     (if (or (< p grid-size)
             (>= p (- grid-length grid-size))
             (= (remainder p grid-size) 0)
             (= (remainder (add1 p) grid-size) 0))
         9
         1))))

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

(define (cell-neighbours r c)
  (define pos (cell-pos (list r c)))
  (map (lambda (offset) (+ pos offset))
       (list (- 0 grid-size 1)
             (- 0 grid-size)
             (- 0 grid-size -1)
             -1
             1
             (+ grid-size -1)
             grid-size
             (+ grid-size 1))))

(grid-set! '(3 2
               3 3
               3 4))

;(for ((i (in-range 10)))
(show-grid)

;(cell-neighbours 2 2)
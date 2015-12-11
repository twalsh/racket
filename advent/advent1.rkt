#lang racket

(define input
  (string->list
   (port->string (open-input-file "input1.txt"))))

(let loop
           (
            (orders input)
            (floor 0)
            (pos 0)
            (first-entry 0)
           )
  (if (empty? orders)
      (cons floor first-entry)
      
      (let ((order (car orders)))
          (loop 
                 (cdr orders)
                 (if (eq? order #\()
                     (+ floor 1)
                     (- floor 1))
                 (+ pos 1)
                 (if (and (= floor -1) (= first-entry 0))
                     pos
                     first-entry)))))
                  
      
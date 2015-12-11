#lang racket

(define input
  (string->list
   (port->string (open-input-file "input3.txt"))))

(define (visit-map order-list init-visits)
  ;(display order-list)(newline)
  (let loop
    (
     (x 0)
     (y 0)
     (visits init-visits)
     (orders order-list))
    (if (empty? orders)
        visits
        (let ((order (car orders)))
          (let ((x (cond ((eq? order #\<) (- x 1))
                         ((eq? order #\>) (+ x 1))
                         (else x)))
                (y (cond ((eq? order #\v) (- y 1))
                         ((eq? order #\^) (+ y 1))
                         (else y))))
            
            (loop x y (hash-update visits (cons x y) add1 0) (cdr orders)))))))

(define (places-visited visit-map)
  (length (filter (lambda (h)
                    (>= (cdr h) 1))
                  (hash->list visit-map
                   
                   ))))
; 3a answer
(places-visited (visit-map input (make-immutable-hash)))

; 3b
(define split-orders
  (let loop ((santa '())
             (robo-santa '())
             (orders input))
    (if (empty? orders)
        (list (reverse santa) (reverse robo-santa))
        (loop (cons (car orders) santa)
              (cons (cadr orders) robo-santa)
              (cddr orders)))))

(define santa-visits (visit-map (car split-orders) (make-immutable-hash)))
(define all-visits (visit-map (cadr split-orders) santa-visits))

; 3b answer
(places-visited all-visits)


      


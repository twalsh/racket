#lang racket


(define (resign) (display "RESIGN Hammurabi: I cannot do what you wish.\nGet yourself another steward!!!"))

(define (prompt message is_valid? fail_msg)
  (let loop ((msg message))
    (displayln msg)
    (let ((amt (read)))
      (if (is_valid? amt)
          amt
          (loop fail_msg)))))

(define _yield 3)

(define (plague pop) (if (< (random) 0.15)
                         (/ pop 2)
                         0))

(define (acres harvest _yield) (/ harvest _yield))

(define (new-born pop acres grain) (exact-round (+ (/ (/ (* (random 6) (+ (* 20 acres) grain)) pop) 100) 1)))

(define (report-feed feed)
  (printf "feed ~a, sufficient for ~a people~n" feed (/ feed 20)))

(define (new-year year pop acres grain planted harvested ratfood born diseased starved)
  (if (<= year 10)
      (let ((feed (distribute-grain pop acres grain)))
        (let ((price (+ 1 (random 25))))
          (let ((d-acres (trade-acres pop acres grain price)))
            (letrec ((planted (plant-crops pop acres grain))
                     (harvested (+ (* planted _yield)))
                     (round (* harvested (random) 0.2)))
              (letrec ((diseased (plague pop))
                       (born (new-born pop acres grain))
                       (starved (max (- pop (/ feed 20)) 0)))
                (new-year (+ year 1)
                          (- (+ pop born) (+ starved diseased))
                          (+ acres d-acres)
                          (- grain (* d-acres price))
                          planted harvested ratfood
                          born diseased starved))))))
      (end-game pop acres total-deaths)))

(define (distribute-grain pop acres grain)
  (prompt (format "How many bushels do you wish to feed your people (0-~a)?" grain)
          (lambda (n) (<= n grain))
          (format "Hammurabi: Think again. You have only ~a bushels of grain." grain)))

(define (trade-acres pop acres grain price)
  (let ((buy
         (prompt (format "How many acres do you wish to buy (0-~a)" (floor (/ grain price)))
                 (lambda (n) (<= (* n price) grain))
                 (format "Hammurabi: Think again. You have only ~a bushels of grain." grain))))
    (if (> buy 0)
        buy
        (let ((sell 
               (prompt (format "How many acres do you wish to sell (0-~a)?" acres)
                       (lambda (n) (<= n acres))
                       (format "Hammurabi: Think again. You own only ~a acres." acres))))
          (if (> sell 0)
              (- sell)
              0)))))

(define (plant-crops pop acres grain)
  (let ((limit (min acres (* grain 2) (* pop 10))))
    (let loop ((msg (format "How many acres do you wish to plant (0-~a)?" limit)))
      (displayln msg)
      (let ((planted (read)))
        (cond ((< planted 0) (resign))
              ((> planted acres) (loop (format "Hammurabi: Think again. You own only ~a acres." acres)))
              ((> (/ planted 2) grain) (loop (format "Hammurabi: Think again. You have only ~a bushels." grain)))
              ((> planted (* 10 pop)) (loop (format "Hammurabi: But you have only ~a people to tend the fields!" pop)))
              (else planted)
              )))))

(define (mortality-rate year starved pop) (exact-round (/ (+ (* (- year 1) mortality-rate) (/ (* starved 100) pop)) year)))

(define (end-game year pop acres total-deaths)
  (let ((avg-mortality-rate (mortality-rate year total-deaths pop)))
  (printf
   "In your ~s-year term of office, ~s percent of the
population starved per year on the average, i.e. a total of
~s people died."  year mortality-rate total-deaths)
  
  (let ((acres-per-person (/ acres pop)))
    (cond ((> avg-mortality-rate 33) (< acres-per-person 7))
          (printf "Owing to extreme mismanagement you have been impeached")
          ((or (> avg-mortality-rate 10) (< acres-per-person 9))
           (printf "Your heavy-handed performance smacks of Nero and Ivan IV.
The people (remaining) find you an unpleasant ruler"))
          ((or (> avg-mortality-rate 3) (< acres-per-person 10))
           (let ((haters (* pop (random) 0.8)))
             (printf "Performance could have been somewhat better, ~s people
would like to see you assassinated" haters)))
          (else
           (printf "Excellent performance"))))))

(define (hammurabi)
  (new-year 1 2 3 4 5 6 7 8 9 10))
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

(define (mortality-rate year starved pop) (exact-round (/ (+ (* (- year 1) mortality-rate) (/ (* starved 100) pop)) year)))

(define (plague pop) (if (< (random) 0.15)
                         (/ pop 2)
                         0))

(define (acres harvest _yield) (/ harvest _yield))

(define (new-born pop acres grain) (exact-round (+ (/ (/ (* (random 6) (+ (* 20 acres) grain)) pop) 100) 1)))

(define (end-game 'Done))

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
      (end-game)))

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

;
;    print()
;    print('In your {}-year term of office, {} percent of the\n'
;          'population starved per year on the average, i.e. a total of\n'
;          '{} people died!!\n'.format(year, int(round(mortality_rate)), total_deaths))
;
;    if impeached or mortality_rate > 33 or acres_per_person < 7:
;        print('Due to your extreme mismanagement you have not only\n'
;              'been impeached and thrown out of office but you have\n'
;              'also been declared national fink!!!!')
;    elif mortality_rate > 10 or acres_per_person < 9:
;        print('Your heavy-handed performance smacks of Nero and Ivan IV.\n'
;              'The people (remaining) find you an unpleasant ruler, and,\n'
;              'frankly, hate your guts!!')
;    elif mortality_rate > 3 or acres_per_person < 10:
;        haters = int(pop * random() * 0.8)
;        print('Your performance could have been somewhat better, but\n'
;              'really wasn\'t too bad at all. {} people\n'
;              'dearly like to see you assassinated but we all have our\n'
;              'trivial problems.'.format(haters))
;    else:
;        print('A fantastic performance!!! Charlemagne, Disraeli and\n'
;              'Jefferson combined could not have done better!')
;
;    print('\nSo long for now.')

(hammurabi)
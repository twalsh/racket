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

(define  (report year harvest planted total-deaths grain ratfood pop starved births diseased acres)
  (printf "YEAR ~a~n" year)
  (printf "ACRES ~a " acres)
  (printf "HARVEST ~a " harvest)
  (printf "PLANTED ~a " planted)
  (printf "RATFOOD ~a " ratfood)
  (printf "GRAIN ~a~n" grain)
  (printf "POPULATION ~a " pop)
  (printf "BIRTHS ~a " births)
  (printf "DISEASE ~a " diseased)
  (printf "STARVED ~a~n" starved)
  )

(define (mortality-rate year starved pop) (exact-round (/ (+ (* (- year 1) mortality-rate) (/ (* starved 100) pop)) year)))

(define (plague pop) (if (< (random) 0.15)
                         (/ pop 2)
                         0))

(define (acres harvest _yield) (/ harvest _yield))

(define (new-born pop acres grain) (exact-round (+ (/ (/ (* (random 6) (+ (* 20 acres) grain)) pop) 100) 1)))

(define (end-game) 'Done)

(define kingdom% (class object%
                   (super-new)
                   (init-field (_year 1)
                               (_harvest 3000)
                               (_planted 1000)
                               (_total-deaths  0)
                               ((_grain grain) 3000)
                               (_ratfood 0) 
                               ((_pop pop) 100)
                               (_starved 0)
                               (_born 5)
                               (_diseased 0)
                               ((_acres acres) 1000)
                               ((_feed feed) 0)
                               )
                   
                   (define/public (year) _year)
                   (define/public (grain) _grain)
                   (define/public (acres) _acres)
                   (define/public (pop) _pop)
                   (define/public (hatch-dispatch)
                     (let ((diseased (plague _pop)))
                       (set! _born (new-born _pop _acres _grain)))
                     (set! _starved (max (- _pop (/ _feed 20)) 0))
                     (set! _pop (- (+ _pop _born) (+ _starved _diseased))))
                   (define/public (harvest planted)
                     (set! _planted planted)
                     (set! _harvest (* _planted _yield))
                     (set! _ratfood (round (* _harvest (random) 0.2))))
                   (define/public (plant-crops d-acres d-grain)
                     (set! _acres (+ _acres d-acres))
                     (set! _grain (+ _grain d-grain)))
                   ))

(define (report-feed feed)
  (printf "feed ~a, sufficient for ~a people~n" feed (/ feed 20)))

(define (ratfood harvested) (round (* harvested (random) 0.2)))

(define (crop harvest ratfood)
  (- harvest ratfood))

(define (max-acres acres grain pop)
  (min acres (* grain 2) (* pop 10)))

(define (hammurabi)
  (new-year 1 2 3 4 5 6 7 8 9 10))

(define (new-year year pop acres grain planted harvested ratfood diseased born starved)
  (if (<= year 10)
      (distribute-grain year pop acres grain)
      (end-game)))

(define (distribute-grain year pop acres grain)
  (let ( (feed (prompt (format "How many bushels do you wish to feed your people (0-~a)?" grain)
          (lambda (n) (<= n grain))
          (format "Hammurabi: Think again. You have only ~a bushels of grain." grain))))
    (trade-acres year pop acres grain feed)))

(define (trade-acres year pop acres grain feed)
  (let ((price (+ 1 (random 25))))
    (printf "Land is trading at ~a bushels per acre~n" price)
   
      (printf "You have ~a acres~n" acres)
      (let ((buy
             (prompt (format "How many acres do you wish to buy (0-~a)" (floor (/ grain price)))
                     (lambda (n) (<= (* n price) grain))
                     (format "Hammurabi: Think again. You have only ~a bushels of grain." grain))))
        (if (> buy 0)
            (plant-crops year pop (+ acres buy) (- grain (* price buy)) feed)
            (let ((sell 
                   (prompt (format "How many acres do you wish to sell (0-~a)?" acres)
                           (lambda (n) (<= n acres))
                           (format "Hammurabi: Think again. You own only ~a acres." acres))))
              (if (> sell 0)
                  (plant-crops year pop (- acres sell) (+ grain (* sell price)) feed)
                  (plant-crops year pop acres grain feed)
              ))
       
        ))))

(define (plant-crops year pop acres grain feed)
    (let ((limit (max-acres acres grain pop)))
      (let loop ((msg (format "How many acres do you wish to plant (0-~a)?" limit)))
        (displayln msg)
        (let ((planted (read)))
          (cond ((< planted 0) (resign))
                ((> planted acres) (loop (format "Hammurabi: Think again. You own only ~a acres." acres)))
                ((> (/ planted 2) grain) (loop (format "Hammurabi: Think again. You have only ~a bushels." grain)))
                ((> planted (* 10 pop)) (loop (format "Hammurabi: But you have only ~a people to tend the fields!" pop)))
                (else (harvest year pop acres grain feed planted)
                      ))))))

(define (harvest year pop acres grain feed planted)
  (let ((harvested (+ (* planted _yield))))
    (hatch-dispatch year pop acres grain feed planted harvested ratfood)))

(define (hatch-dispatch year pop acres grain feed planted harvested ratfood)
  (let ((diseased (plague pop)))
    (let ((born (new-born pop acres grain)))
      (let ((starved (max (- pop (/ feed 20)) 0)))
        (let ((pop (- (+ pop born) (+ starved diseased))))
        (new-year (+ year 1)
                      pop acres  grain planted harvested ratfood diseased born starved))))))


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
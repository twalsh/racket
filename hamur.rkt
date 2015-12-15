#lang racket


(define (resign) (display "RESIGN Hammurabi: I cannot do what you wish.\nGet yourself another steward!!!"))
;
;
(define (prompt message is_valid? fail_msg)
  (let loop ((msg message))
    (displayln msg)
    (let ((amt (read)))
      (if (is_valid? amt)
          amt
          (loop fail_msg)))))

(define _yield 3)

(define (distribute-grain grain)
  (prompt (format "How many bushels do you wish to feed your people (0-~a)?" grain)
          (lambda (n) (<= n grain))
          (format "Hammurabi: Think again. You have only ~a bushels of grain." grain)))

(display "Try your hand at governing ancient Sumeria for a ten-year term of office.")

(define (mortality-rate year starved pop) (exact-round (/ (+ (* (- year 1) mortality-rate) (/ (* starved 100) pop)) year)))

(define (plague pop) (if (< (random) 0.15)
                         (/ pop 2)
                         0))

(define (acres harvest _yield) (/ harvest _yield))

(define (new-born pop acres grain) (exact-round (+ (/ (/ (* (random 6) (+ (* 20 acres) grain)) pop) 100) 1)))

(define (end-game) 'Done)

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

(define (report-feed feed)
   (printf "feed ~a, sufficient for ~a people~n" feed (/ feed 20)))

(define (plant-crops acres grain pop feed)
  (let ((limit (min acres (* grain 2) (* pop 10))))
    (let loop ((msg (format "How many acres do you wish to plant (0-~a)?" limit)))
      (displayln msg)
      (let ((planted (read)))
        (printf "PLANTED ~a~n" planted)
        (cond ((< planted 0) (resign))
              ((> planted acres) (loop (format "Hammurabi: Think again. You own only ~a acres." acres)))
              ((> (/ planted 2) grain) (loop (format "Hammurabi: Think again. You have only ~a bushels." grain)))
              ((> planted (* 10 pop)) (loop (format "Hammurabi: But you have only ~a people to tend the fields!" pop)))
              (else (harvest planted acres grain pop feed)
              ))))))

(define (harvest planted acres grain pop feed)
  (letrec ((harvest (* planted _yield))
           (ratfood (round (* harvest (random) 0.2)))
           )
    (hatch-dispatch ratfood planted acres grain pop feed)))

(define (hatch-dispatch ratfood planted acres grain pop feed)
  (let ((diseased (plague pop))
        (born (new-born pop acres grain))
        (starved (max (- pop (/ feed 20)) 0))
        )
    (let ((new-pop (- (+ pop born) (+ starved diseased))))
      (new-year new-pop born starved diseased ratfood planted acres grain pop feed))))

(define (trade-acres acres grain pop feed)
  (let ((price (+ 1 (random 25))))
    (printf "Land is trading at ~a bushels per acre~n" price)
    (printf "You have ~a acres~n" acres)
    (let ((buy
           (prompt (format "How many acres do you wish to buy (0-~a)" (floor (/ grain price)))
                   (lambda (n) (<= (* n price) grain))
                   (format "Hammurabi: Think again. You have only ~a bushels of grain." grain))))
      (if (> buy 0)
           (plant-crops (+ acres buy) (- grain (* price buy)) pop feed)
           (let ((sell 
                  (prompt (format "How many acres do you wish to sell (0-~a)?" acres)
                          (lambda (n) (<= n acres))
                          (format "Hammurabi: Think again. You own only ~a acres." acres))))
             (if (> sell 0)
                 (plant-crops (- acres sell) (+ grain (* sell price)) pop feed)
                 (plant-crops acres grain pop feed))
                   )))))

(define (new-year year harvest planted total-deaths old-grain ratfood pop starved born diseased acres)
      (report year harvest planted total-deaths old-grain ratfood pop starved born diseased acres)
      (if (<= year 10)
          (let ((feed (distribute-grain grain)))
            (trade-acres acres grain plant-crops pop feed))
          (end-game)))

(define (hammurabi)
  (new-year (hash 'year 1
            'harvest 3000
             'planted 1000
             'total-deaths  0
             'grain 3000
             'ratfood 0 
             'pop 100
             'starved 0
             'born 5
             'diseased 0
             'acres 1000
             )))

    
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
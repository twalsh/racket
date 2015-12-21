#lang racket
                        ; 0 1 2 3 4  5 6  7 8  9
(define simple-index  #( 50 0 1 7 2 23 8 33 3 14 ))

(define product-table (hash 0 1 1 2 2 4 3 8 4 16 5 32 6 64 7 3 8 6 9 12
                              10 24 11 48 12 -1 13 -1 14 9 15 18 16 36 17 72
                              18 -1 19 -1 20 -1 21 27 22 54 23 5 24 10 25 20
                              26 40 27 -1 28 81 29 -1 30 15 31 30 32 -1 33 7
                              34 14 35 28 36 56 37 45 38 -1 39 -1 40 21 41 42
                              42 -1 43 -1 44 -1 45 -1 46 25 47 63 48 -1 49 -1
                              50 0
                              51 2
                              52 4
                              53 8
                              54 -1 55 -1 56 35
                              57 3
                              58 6
                              59 -1 60 -1 61 -1 62 -1 63 -1
                              64 9
                              65 -1 66 49
                              73 5
                              83 7
                              100 1 ))

(define (num2digits n0)
  (let loop ((digits '()) (n n0))
    (if (>= n 1)
        (loop (cons (remainder n 10) digits) (floor (/ n 10)))
        digits
        )))
  
(define n1 523)

(define d1 (num2digits n1))

(printf "~s ~s~n" n1 d1)

(define (shift products)
  (for/list ((i (in-range (length products)))
             (p (reverse products)))
   (* p (expt 10 i))))
  
(define (partial-product d1 digits)
     (let loop ((sum 0)
                (shift 1)
                (pprod
                 (reverse
                  (for/list ((d2 digits))
                  (let ((i1 (vector-ref simple-index d1))
                        (i2 (vector-ref simple-index d2)))
                    (let ((ci (+ i1 i2)))
                      (hash-ref product-table ci))
                    )))))
       (if (empty? pprod)
           sum
           (loop
            (+ sum (* (first pprod) shift))
            (* shift 10)
            (rest pprod)))))

(define (mul n1 n2)
  (printf "MUL ~s ~s:~n" n1 n2)
  (let ((digits1 (num2digits n1))
        (digits2 (num2digits n2)))
    (apply + (shift (map (lambda (d) (partial-product d digits2)) digits1)))))
      
      

(for* ((i (range 1 10))
       (j (range 1 10)))
  (printf "~s ~s ~s ~s~n" i j (* i j) (mul i j)))

(define i 12)
(define j 21)
(printf "MD ~s MR ~s E ~s A ~s~n" i j (* i j) (mul i j))

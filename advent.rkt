#lang racket

(define file "/home/tom/advent/adventure.text")

(define number-of-sections 14)

(define sections (make-vector (+ 1 number-of-sections)))

(define travel (make-hash))
(define vocab (make-hash))
(define object-location (make-hash))
(define action-defaults (make-hash))
(define location-attributes (make-hash))
(define class-messages (make-hash))
; 11
(define hints (make-hash))
; 13
(define sound-text (make-hash))
; 14
(define turn-thresholds (make-hash))

(define (read-section)
  (let ((header (read-line)))
    (if (string=? header "0")
        '()
        (let while ((lines '()))
          (let ((line (read-line)))
            (if (string=? line "-1")
                (list header (reverse lines))
                (while (cons line lines))))))))

(define main
  (with-input-from-file file
    (lambda () 
      (do ([section (read-section) (read-section)])
        ((null? section))
        (let ((header (string->number (car section))) (data (cadr section)))
          (vector-set! sections header data))))))

(define section-data (make-vector 15))

(define (process-section-1 fields table)
   (let ((locn (string->number (car fields))) 
        (text (string-join (rest fields) " ")))
    (let* ((old-text (vector-ref table locn))
           (new-text
            (if (string=? old-text "")
                text
                (string-append old-text "\n" text)))
           )
      (vector-set! table locn new-text))))

(define (process-section-3 fields table)
  (let* ((fieldn (map string->number fields))
         (locx (car fieldn))
         (locy (cadr fieldn))
         (motion (drop fieldn 2)))
    (let ((idx (hash-ref table locx (make-hash))))
      (hash-set! idx locy motion)
      (hash-set! table locx idx)
      )))

(define (process-section-4 fields table)
  (let ((class (string->number (first fields)))
        (word (string-downcase (second fields))))
    (hash-set! table word class)))

(define (process-section-7 fields table)
  ; object locations
  (let* ((fieldn (map string->number fields))
         (object (first fieldn))
         (loc (rest fieldn)))
    (hash-set! table object loc)))

(define (process-section-8 fields table)
  ; action defaults (8) 
  (let* ((fieldn (map string->number fields))
         (action-verb (first fieldn))
         (message (second fieldn)))
    (hash-set! table action-verb message)))

(define (process-section-9 fields table)
  ; location attributes
  (let* ((fieldn (map string->number fields))
         (attr (first fieldn))
         (location (second fieldn)))
    (hash-set! table attr location)))

(define (process-section-10 fields table)
  ; class messages (10)
  (let ((class (string->number (first fields)))
        (message (string-join (rest fields) " ")))
    (hash-set! table class message)))

(define (process-section-11 fields table)
  ; hints (11)
  (let* ((fieldn (map string->number fields))
         (hint (first fieldn))
         (data (rest fieldn)))
    (hash-set! table hint data)))

(define (process-section section section-func table) 
  (for-each (lambda (x)
         (let ((fields (regexp-split #px"[[:blank:]]+" x)))
           (section-func fields table)))
       section))

(define (process-section-5 section)
  (let loop ((next-line (car section)) (data (cdr section)))
    (let* ((fields (regexp-split #rx"\t" next-line))
           (index (string->number (car fields)))
           (text (cdr fields)))
      (if (< index 100)
          

(define long-desc (make-vector 200 ""))
(process-section (vector-ref sections 1) process-section-1 long-desc)
(define short-desc (make-vector 200 ""))
(process-section (vector-ref sections 2) process-section-1 short-desc)
(process-section (vector-ref sections 3) process-section-3 travel)
(process-section (vector-ref sections 4) process-section-4 vocab)
;
(define arbitary (make-vector 300 ""))
(process-section (vector-ref sections 6) process-section-1 arbitary)
(process-section (vector-ref sections 7) process-section-7 object-location)
(process-section (vector-ref sections 8) process-section-8 action-defaults)
(process-section (vector-ref sections 9) process-section-9 location-attributes)
(process-section (vector-ref sections 10) process-section-10 class-messages)
(process-section (vector-ref sections 11) process-section-11 hints)
(process-section (vector-ref sections 13) process-section-11 sound-text)
(process-section (vector-ref sections 14) process-section-10 turn-thresholds)

;(display (vector-ref section-data 3))
(display travel)
(display vocab)
(display arbitary)
(newline)
(display object-location)
(display location-attributes)
(newline)
(display class-messages)
(newline)
(display hints)
(newline)
(display turn-thresholds)
(newline)
(display sound-text)
(newline)
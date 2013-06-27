#lang racket
(require srfi/1)

(define cipher-text "BT JPX RMLX PCUV AMLX ICVJP IBTWXVR CI M LMT’R PMTN, MTN YVCJX CDXV MWMBTRJ JPX AMTNGXRJBAH UQCT JPX QGMRJXV CI JPX YMGG CI JPX HBTW’R QMGMAX; MTN JPX HBTW RMY JPX QMVJ CI JPX PMTN JPMJ YVCJX. JPXT JPX HBTW’R ACUTJXTMTAX YMR APMTWXN, MTN PBR JPCUWPJR JVCUFGXN PBL, RC JPMJ JPX SCBTJR CI PBR GCBTR YXVX GCCRXN, MTN PBR HTXXR RLCJX CTX MWMBTRJ MTCJPXV. JPX HBTW AVBXN MGCUN JC FVBTW BT JPX MRJVCGCWXVR, JPX APMGNXMTR, MTN JPX RCCJPRMEXVR. MTN JPX HBTW RQMHX, MTN RMBN JC JPX YBRX LXT CI FMFEGCT, YPCRCXDXV RPMGG VXMN JPBR YVBJBTW, MTN RPCY LX JPX BTJXVQVXJMJBCT JPXVXCI, RPMGG FX AGCJPXN YBJP RAM")

(define cipher-list (filter char-alphabetic? (string->list cipher-text)))

(define alphabet (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define freq-table (make-hash))

(for ((c cipher-list))
  (hash-update! freq-table c (lambda (i) (+ i 1)) 0)  
  )

(define freq-list (hash->list freq-table))

(define sort-by-freq (lambda (a b) (> (cdr a) (cdr b))))

(define sorted-freq (sort freq-list sort-by-freq))

; This was found by trial and error.
(define most-frequent (string->list "ETAHNOSIRDLGCWFKMUPBYVJ"))

(define cipher-alphabet (map car sorted-freq))

(define cipher->plain (make-hash (zip cipher-alphabet most-frequent)))
 
(for ((c (string->list cipher-text))) 
  (let ((p (hash-ref cipher->plain c '())))
    (if (null? p)
        (printf "~a" c)
        (printf "~a" (car p))
        )))








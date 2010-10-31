#lang racket/gui

 ; ... pens, brushes, and draw-face are the same as above ...
  
  ; Create a 300 x 300 bitmap
  (define face-bitmap (make-object bitmap% 300 300))
  ; Create a drawing context for the bitmap
  (define bm-dc (make-object bitmap-dc% face-bitmap))
  ; A bitmap's initial content is undefined; clear it before drawing
  (send bm-dc clear)
 
  
  ; Make a 300 x 300 frame
  (define frame (new frame% [label "Drawing Example"]
                            [width 300]
                            [height 300]))
  
  ; Make a drawing area whose paint callback copies the bitmap
  (define canvas
    (new canvas% [parent frame]
                 [paint-callback
                  (lambda (canvas dc)
                    (send dc draw-bitmap face-bitmap 0 0))]))
 ; Make some pens and brushes
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define no-brush (make-object brush% "WHITE" 'solid))
  (define brush (make-object brush% "BLACK" 'solid))
  (define pen (make-object pen% "BLACK" 2 'solid))
  
   (define dc (send canvas get-dc))
  
(send frame show #t)
  ; Wait a second to let the window get ready
  (sleep/yield 1)
   
  (send dc set-pen pen)
  ;  (send dc set-brush brush)
 ;   (send dc draw-rectangle 0 0 10 10)
;(sleep/yield 10)
 ;(send dc set-pen no-pen)
;(send dc set-brush no-brush)
    ;(send dc draw-rectangle 0 0 10 10)
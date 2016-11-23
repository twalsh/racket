#lang racket

(define game-map
  #hash(
        (living-room . ((You are in the living-room of a house. There is a wizard snoring loudly on the couch.)
                        (west door garden)
                        (upstairs stairway attic)))
        (garden . ((You are in a beautiful garden. There is a well in front of you.)
                   (east door living-room)))
        (attic . ((You are in the attic of the wizard's house. There is a giant welding torch in the corner.)
                  (downstairs stairway living-room))
   
               )))

(struct object (name location))

(define objects (map (lambda (o) (object (first o) (second o)))
                     '((whiskey-bottle living-room)
                       (bucket living-room)
                       (chain garden)
                       (frog garden))))

(define location 'living-room)

(define (describe-location location game-map)
  (first (hash-ref game-map location)))

(define (describe-path path)
  `(There is a ,(second path) going ,(first path) from here.))

(define (describe-paths location game-map)
  (flatten (map describe-path (rest (hash-ref game-map location)))))

(define (is-at? obj location)
  (eq? (object-location obj) location))

(define (describe-floor location objects)
  (map (lambda (o)
         `(There is a ,(object-name o) on the floor.))
         (filter (lambda (o) (is-at? o location)) objects)))


(define (look location game-map)
  (list
         (describe-location location game-map)
         (describe-paths location game-map)
         (describe-floor location objects)))

(look 'living-room game-map)




       
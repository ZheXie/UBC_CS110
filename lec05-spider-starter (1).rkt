;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lec05-spider-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

;; lec05-spider-starter.rkt

#|
PROBLEM:

Design a world program in which a spider starts at the top of the screen
and slowly drops down it. The spider should stop when it reaches the bottom
of the screen.

You can improve your spider by re-running the HtDW recipe to add these
features. 


  - Draw a line from the top of the screen to the spider, this is the thread 
    it is hanging from. You will need to use add-line for this. Look in the
    DrRacket help desk to see how add-line works.
    
  - Arrange for pressing the space key to reset the spider to the top of 
    the screen.
|#

;(spider moving down the screen)

;; Spider  (make this more specific)

(@htdw Spider)

;; =================
;; Constants:
(define spider-radius 10)
(define spider-image (circle spider-radius "solid" "black"))
(define speed 2) ;pixels per tick
(define WIDTH 400)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))
(define spider-x (/ WIDTH 2))
(define S-TOP spider-radius)
(define S-MID (/ HEIGHT 2))
(define S-BOT (- HEIGHT spider-radius))


;; =================
;; Data definitions:

(@htdd Spider)
;; spider is Number
;; interp. Number is the y coordination of the spider image
;; CONSTRAINT: to be visible must be in [spider-radius, HEIGHT-spider-radius]
(@dd-template-rules atomic-non-distinct)
;(define fn-for-spider (... s))

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main S-TOP)
;;

(@template htdw-main)

(define (main s)
  (big-bang S-TOP                    ; Spider
            (on-tick   tock)     ; Spider -> Spider
            (to-draw   render))) ; Spider -> Image
            



(@htdf tock)
(@signature Spider -> Spider)
;; produce the next spider by increasing its y by speed
;; !!!
(check-expect (tock spider-radius) (+ spider-radius speed))
(check-expect (tock (/ HEIGHT 2)) (+ (/ HEIGHT 2) speed))
(check-expect (tock (- HEIGHT spider-radius)) (- HEIGHT spider-radius))


;(define (tock s) 0)

(@template Spider)

(define (tock s)
  (if (<= (+ s speed) (- HEIGHT spider-radius))
      (+ s speed)
      (- HEIGHT spider-radius)))




(@htdf render)
(@signature Spider -> Image)
;; produce the spider-image on MTS at spider-x the spider's y 
;; !!!
(check-expect (render S-TOP) (place-image spider-image spider-x S-TOP MTS))
(check-expect (render S-MID) (place-image spider-image spider-x S-MID MTS))
(check-expect (render S-BOT) (place-image spider-image spider-x S-BOT MTS))

;(define (render s) empty-image)

(@template Spider)

(define (render s)
  (place-image spider-image spider-x s MTS))

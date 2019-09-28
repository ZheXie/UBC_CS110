;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec05-spider-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

;; lec05-spider-v1.rkt

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



;; My creepy crawly spider

(@htdw Spider)

;; =================
;; Constants:

(define WIDTH  400) ;pixels
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))

(define SPIDER-RADIUS 10)

(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))

(define SPEED 2) ; pixels per tick

(define MTS (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:

(@htdd Spider)
;; Spider is Number
;; interp. y coordinate of the spider
;;         distance of the centre of the spider from top
;; CONSTRAINT: to be visible, must be in
;;             [SPIDER-RADIUS, HEIGHT - SPIDER-RADIUS]
(define S-TOP SPIDER-RADIUS)
(define S-MID (/ HEIGHT 2))
(define S-BOT (- HEIGHT SPIDER-RADIUS))

(@dd-template-rules atomic-non-distinct)

(define (fn-for-spider s)
  (... s))

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main S-TOP)
;;
(@template htdw-main)

(define (main s)
  (big-bang s                     ; Spider
            (on-tick   tock)      ; Spider -> Spider
            (to-draw   render)    ; Spider -> Image
            ;(stop-when ...)      ; Spider -> Boolean
            ;(on-mouse  ...)      ; Spider Integer Integer MouseEvent -> Spider
            ;(on-key    ...)      ; Spider KeyEvent -> Spider
    ))


(@htdf tock)
(@signature Spider -> Spider)
;; produce the next spider by adding SPEED to s, stopping at bottom
(check-expect (tock SPIDER-RADIUS) (+ SPIDER-RADIUS SPEED))
(check-expect (tock (- S-BOT SPEED 1)) (- S-BOT 1))
(check-expect (tock (- S-BOT SPEED 0)) (- S-BOT 0))
(check-expect (tock (+ (- S-BOT SPEED) 1)) S-BOT)

;(define (tock s) s) ;stub

(@template Spider)

(define (tock s)
  (if (>= (+ s SPEED) S-BOT)
      S-BOT
      (+ s SPEED)))


(@htdf render)
(@signature Spider -> Image)
;; place SPIDER-IMG on MTS at proper x and y 
(check-expect (render S-TOP) (place-image SPIDER-IMAGE CTR-X S-TOP MTS))
(check-expect (render S-MID) (place-image SPIDER-IMAGE CTR-X S-MID MTS))

;(define (render s) MTS) ;stub

(@template Spider)

(define (render s)
  (place-image SPIDER-IMAGE CTR-X s MTS))
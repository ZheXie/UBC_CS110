;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |pset-03-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment pset-03);Do not edit or remove this tag
(@cwl xiezhe12)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one

;; Bounce a single ball around the screen.

;; IMPORTANT:
;;   This problem set will form the basis for problem sets 4 and 5 as well.
;;   You are strongly advised to put in the effort to do well on this
;;   problem set so that you then have an easier time with problem sets
;;   4 and 5.
;;

;; For this problem set you must complete the design of a world program.
;; The complete program must have the following behaviour:
;;   - there is a green background, with a single white ball
;;     bouncing around inside that box
;;   - when the ball hits the top/right/bottom/left of the box it
;;     bounces in the usual way a ball would bounce off of a wall
;;   - clicking the mouse replaces the ball with a new ball, at the
;;     current mouse position, and with a small random x,y velocity
;;
;; Note that we are giving you significant help in the starter file.
;; There is a complete data design, some complete helper functions, as
;; well as a some wish list entries.  You absolutely MUST
;;  - not edit any of the complete design elements we give you
;;  - complete the wish list entries we give you, and do so in a way
;;    that is consistent with the rest of the design we give you
;;  - must use the helpers we give you as part of your solution
;;
;; We recommend that you begin by first going through the entire file
;; understand what is already complete and what you need to complete.
;;
(@problem 1)
(@htdw Ball)

;; Constants:
(define WIDTH  605)
(define HEIGHT 535)


(define BALL-RADIUS 10)

(define BALL (circle BALL-RADIUS "solid" "white"))

(define MTS (rectangle WIDTH HEIGHT "solid" "green"))


;; ===========================================================================
;; ===========================================================================
;; Data definitions:

(@htdd Ball)

(define-struct ball (x y dx dy))
;; Ball is (make-ball Number Number Number Number)
;; interp. (make-ball x y dx dy) is ball
;;   - position x, y    in screen coordinates
;;   - velocity dx, dy  in pixels/tick
(define B1 (make-ball (- WIDTH 1) (/ HEIGHT 2) -4 -4))

(@dd-template-rules compound)

(define (fn-for-ball b)
  (... (ball-x b) 
       (ball-y b) 
       (ball-dx b) 
       (ball-dy b)))



;; ===========================================================================
;; ===========================================================================
;; Functions:

(@htdf main)
(@signature Ball -> Ball)
;; start the game, call with (main B1)
;; <no tests for main functions>

(@template htdw-main)

(define (main b)
  (big-bang b
            (on-draw   render-ball)   ;Ball -> Image
            (on-tick   next-ball)     ;Ball -> Ball
            (on-mouse  handle-mouse)));Ball MouseEvent Integer Integer -> Ball
            



(@htdf render-ball)
(@signature Ball -> Image)
;; place BALL on image at appropriate x, y coordinate
;; !!!
(check-expect (render-ball B1)
              (place-image BALL (- WIDTH 1) (/ HEIGHT 2) MTS))
(check-expect (render-ball (make-ball 300 200 1 1))
              (place-image BALL 300 200 MTS))
#;
(define (render-ball b) empty-image)

(@template Ball)

(define (render-ball b)
  (place-image BALL (ball-x b) (ball-y b) MTS))



(@htdf next-ball)
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall
(check-expect (next-ball (make-ball 100 3 3 -4))
              (bounce-top (make-ball 100 3 3 -4)))
(check-expect (next-ball (make-ball 100 HEIGHT 3 4)) 
              (bounce-bottom (make-ball 100 HEIGHT 3 4)))
(check-expect (next-ball (make-ball 0 100 -3 1)) 
              (bounce-left (make-ball 0 100 -3 1)))
(check-expect (next-ball (make-ball WIDTH 100 3 4)) 
              (bounce-right (make-ball WIDTH 100 3 4)))
(check-expect (next-ball (make-ball 100 110 3 4)) 
              (glide (make-ball 100 110 3 4)))
#;
(define (next-ball b) b)

(@template Number) ;(@template Number) because b is treated as atomic

(define (next-ball b)
  (cond [(touch-top?    b) (bounce-top b)]
        [(touch-bottom? b) (bounce-bottom b)]
        [(touch-right?  b) (bounce-right b)]
        [(touch-left?   b) (bounce-left b)]
        [else
         (glide b)]))


(@htdf handle-mouse)
(@signature Ball Integer Integer MouseEvent -> Ball)
;; replace ball with new ball on mouse click
;; NOTE: uses random, so testing has to use check-random
(check-random (handle-mouse (make-ball 1 2 3 4) 100 200 "button-down")
              (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11))))
(check-random (handle-mouse (make-ball 1 2 3 4) 100 200 "button-up")
              (make-ball 1 2 3 4))
#;
(define (handle-mouse b x y me) b)

(@template MouseEvent)

(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down")
         (make-ball x y (- 5 (random 11)) (- 5 (random 11)))]
        [else b]))
        

(@htdf touch-top?)
(@signature Ball -> Boolean)
;; true if ball is going up and CENTER hits top edge of box
(check-expect (touch-top?    (make-ball 1  100 3 -4)) false)
(check-expect (touch-top?    (make-ball 1  3   3 -2)) false)
(check-expect (touch-top?    (make-ball 1  2   3 -2)) true)
(check-expect (touch-top?    (make-ball 1  1   3 -4)) true)
#;
(define (touch-top? b) false)

(@template Ball)

(define (touch-top? b) 
  (<= (+ (ball-y b) (ball-dy b)) 0))


(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; true if ball is going down and CENTER hits bottom edge of box
;; !!!
(check-expect (touch-bottom? (make-ball 300 530  3 4)) false)
(check-expect (touch-bottom? (make-ball 300 531 -3 3)) false)
(check-expect (touch-bottom? (make-ball 300 531  3 4)) true)
(check-expect (touch-bottom? (make-ball 300 532  3 4)) true)
#;
(define (touch-bottom? b) false)

(@template Ball)

(define (touch-bottom? b)
  (>= (+ (ball-y b) (ball-dy b)) 535))


(@htdf touch-left?)
(@signature Ball -> Boolean)
;; true if ball is going left and CENTER hits left edge of box
;; !!!
;; !!!
(check-expect (touch-left? (make-ball 5 100 -3  4)) false)
(check-expect (touch-left? (make-ball 5 100 -4 -4)) false)
(check-expect (touch-left? (make-ball 5 100 -5  4)) true)
(check-expect (touch-left? (make-ball 4 100 -5  4)) true)
#;
(define (touch-left? b) false)

(@template Ball)

(define (touch-left? b)
  (<= (+ (ball-x b) (ball-dx b)) 0))

(@htdf touch-right?)
(@signature Ball -> Boolean)
;; true if ball is going right and CENTER hits right edge of box
;; !!!
(check-expect (touch-right? (make-ball 600 300 3  4)) false)
(check-expect (touch-right? (make-ball 600 300 4 -4)) false)
(check-expect (touch-right? (make-ball 600 300 5  4)) true)
(check-expect (touch-right? (make-ball 601 300 5  4)) true)
#;
(define (touch-right? b) false)

(@template Ball)

(define (touch-right? b)
  (>= (+ (ball-x b) (ball-dx b)) 605))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of top edge of the box
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-expect (bounce-top (make-ball 3  0 2 -3))
              (make-ball 3 1 2 3))
(check-expect (bounce-top (make-ball 4 -1 5 -6))
              (make-ball 4 1 5 6))
#;
(define (bounce-top b) b)

(@template Ball)

(define (bounce-top b) 
  (make-ball (ball-x b) 1 (ball-dx b) (- (ball-dy b))))

(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of bottom edge of the box
;; CONSTRAINT: assume ball is close to bottom edge and moving down
;; !!!
(check-expect (bounce-bottom (make-ball 300 531  3 4))
              (make-ball 300 534 3 -4))
(check-expect (bounce-bottom (make-ball 300 530  3 5))
              (make-ball 300 534 3 -5))
#;
(define (bounce-bottom b) b)

(@template Ball)

(define (bounce-bottom b)
  (make-ball (ball-x b) 534 (ball-dx b) (- (ball-dy b))))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of left edge of the box
;; CONSTRAINT: assume ball is close to left edge and moving left
;; !!!
(check-expect (bounce-left (make-ball 5 100 -5  4))
              (make-ball 1 100 5 4))
(check-expect (bounce-left (make-ball 4 100 -6  4))
              (make-ball 1 100 6 4))
#;              
(define (bounce-left b) b)

(@template Ball)

(define (bounce-left b)
  (make-ball 1 (ball-y b) (- (ball-dx b)) (ball-dy b)))

(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of right edge of the box
;; CONSTRAINT: assume ball is close to right edge and moving right
;; !!!
(check-expect (bounce-right (make-ball 600 300 5  4))
              (make-ball 604 300 -5 4))
(check-expect (bounce-right (make-ball 601 300 6  4))
              (make-ball 604 300 -6 4))
#;
(define (bounce-right b) b)

(@template Ball)

(define (bounce-right b)
  (make-ball 604 (ball-y b) (- (ball-dx b)) (ball-dy b)))


(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
;; !!!
(check-expect (glide (make-ball 300 200  2  2))
              (make-ball 302 202  2  2))
(check-expect (glide (make-ball 300 200 -2  2))
              (make-ball 298 202 -2  2))
(check-expect (glide (make-ball 300 200  2 -2))
              (make-ball 302 198  2 -2))
(check-expect (glide (make-ball 300 200 -2 -2))
              (make-ball 298 198 -2 -2))
#;
(define (glide b) b)

(@template Ball)

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b))
             (ball-dx b) (ball-dy b)))


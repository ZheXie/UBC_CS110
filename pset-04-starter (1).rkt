;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |pset-04-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@assignment pset-04);Do not edit or remove this tag
(@cwl xiezhe12)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


;; Below is our solution to Problem Set 3.
;; For problem set 4 you must improve this program in two different ways.
;; We suggest that you fully complete the first improvement before moving on to
;; the second.  Be sure not to change the program's behaviour in any other ways
;; than we are asking.  Making extra improvements could cause you problems
;; with the autograder.
;;
;; CHANGE 1:
;; In problem set 3 we tried to get away with a too simple rule for when
;; balls touch the boundaries.  We made the rule be that when the center of
;; the ball touches the edge of the box it counts as touching.  We were
;; hoping the ball was moving fast enough that you eye wouldn't see that
;; the edge of the ball goes outside the box.  But it didn't work.
;;
;; Please modify the program so that a ball touches an edge when the ball's
;; edge touches.  So the ball touches the top for example when its y coordinate
;; is <= BALL-RADIUS.  Your modification should be systematic and complete.
;; Update not just functions but also tests, comments etc.  You should start
;; by including the following four constants, and adding the following
;; constraint to the Ball data definition.
#|
(define TOP BALL-RADIUS)
(define BOT (- HEIGHT BALL-RADIUS))
(define LEF BALL-RADIUS)
(define RIG (- WIDTH BALL-RADIUS))

;; CONSTRAINT: x is in [LEF, RIG]; y is in [TOP, BOT]
|#

;;
;; CHANGE 2:
;; Modify the program so that instead of there being a single ball there
;; can be any number of balls.  The mouse button should add a new ball.
;; Pressing space should clear all the balls.
;;
;; Again your change should be systematic and complete.  All relevant
;; design elements and tags must be updated.  You must name the new
;; world state type ListOfBall.

(@problem 1)
(@htdw ListOfBall)

;; Constants:
(define WIDTH  605)
(define HEIGHT 535)


(define BALL-RADIUS 10)
(define TOP BALL-RADIUS)
(define BOT (- HEIGHT BALL-RADIUS))
(define LEF BALL-RADIUS)
(define RIG (- WIDTH BALL-RADIUS))
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

(@htdd ListOfBall)
;; ListOfBall is one of
;; - empty
;; - (cons Ball ListOfBall)
;; interp. ListOfBall is a list of motherfucking ball

(define LOB1 empty)
(define LOB2 (cons (make-ball 20 20 2 2) (cons B1 empty)))

(@dd-template-rules one-of          ;2 cases
                    atomic-distinct ;empty
                    compound        ;(cons Ball ListOfBall)
                    ref             ;(first ListOfBall) is Ball 
                    self-ref)       ;(rest ListOfBall) is ListOfBall

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-ball (first lob))
              (fn-for-lob  (rest lob)))]))


;; ===========================================================================
;; ===========================================================================
;; Functions:

(@htdf main)
(@signature ListOfBall -> ListOfBall)
;; start the game, call with (main B1)
;; <no tests for main functions>

(@template htdw-main)

(define (main lob)
  (big-bang lob
            (on-draw   render-ball)   ;ListOfBall -> Image
            (on-tick   next-list)     ;ListOfBall -> ListOfBall
            (on-key    handle-key)    ;ListOfBall KeyEvent -> ListOfBall
            (on-mouse  handle-mouse)));ListOfBall MouseEvent Integer Integer ->
                                      ;ListOfBall

            

(@htdf render-ball)
(@signature ListOfBall -> Image)
;; place BALL on image at appropriate x, y coordinate
(check-expect (render-ball empty) MTS)
(check-expect (render-ball (cons (make-ball 20 30 3 3) empty))
              (place-image BALL 20 30 MTS))
(check-expect (render-ball (cons (make-ball (- RIG 4) (- TOP 5) -2 -3)
                                 (cons (make-ball 20 30 3 3) empty)))
              (place-image BALL (- RIG 4) (- TOP 5)
                           (place-image BALL 20 30 MTS)))
#;
(define (render-ball lob) MTS)

(@template ListOfBall)

(define (render-ball lob)
  (cond [(empty? lob) MTS]
        [else
         (place-image BALL (ball-x (first lob))(ball-y (first lob))
                      (render-ball (rest lob)))]))


(@htdf next-ball)
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall
(check-expect (next-ball     (make-ball 11 (+ TOP 4) 3 -4))
              (bounce-top    (make-ball 11 (+ TOP 4) 3 -4)))
(check-expect (next-ball     (make-ball 11 (- BOT 2) 3  2)) 
              (bounce-bottom (make-ball 11 (- BOT 2) 3  2)))
(check-expect (next-ball     (make-ball (+ LEF 5) 11 -5 2))
              (bounce-left   (make-ball (+ LEF 5) 11 -5 2)))
(check-expect (next-ball     (make-ball (- RIG 5) 11  5 2)) 
              (bounce-right  (make-ball (- RIG 5) 11  5 2)))
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
(@signature ListOfBall Integer Integer MouseEvent -> ListOfBall)
;; add a ball on mouse click
;; NOTE: uses random, so testing has to use check-random
(check-random (handle-mouse
               (cons (make-ball 1 2 3 4) empty) 100 200 "button-down")
              (cons (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)))
                    (cons (make-ball 1 2 3 4) empty)))
(check-random (handle-mouse
               (cons (make-ball 1 2 3 4) empty) 100 200 "button-up")
              (cons (make-ball 1 2 3 4) empty))
(check-random (handle-mouse empty)
              (cons (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)))
                    empty)) 
               
#;
(define (handle-mouse lob x y me) lob)

(@template MouseEvent)

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-ball x y (- 5 (random 11)) (- 5 (random 11))) lob)]
        [else lob]))



(@htdf handle-key)
(@signature ListOfBall KeyEvent -> ListOfBall)
;; clear all the ball when press " "
(check-expect (handle-key empty " ") empty)
(check-expect (handle-key (cons (make-ball 20 20 4 4) empty) " ") empty)
(check-expect (handle-key (cons (make-ball 20 20 4 4) empty) "a")
              (cons (make-ball 20 20 4 4) empty))

;(define (handle-key lob) empty)  ;stub

(@template ListOfBall)
(define (handle-key lob ke)
  (cond [(key=? ke " ") empty]
        [else lob]))

(@htdf next-list)
(@signature ListOfBall -> ListOfBall)
;; produce the next list of ball
(check-expect (next-list empty) empty)
(check-expect (next-list (cons (make-ball 20 30 2 2) empty))
              (cons (make-ball 22 32 2 2) empty))
(check-expect (next-list (cons (make-ball 40 50 3 3)
                              (cons (make-ball 20 30 2 2) empty)))
              (cons (make-ball 43 53 3 3)
                    (cons (make-ball 22 32 2 2) empty)))
              
;(define (next-list lob) empty)  ;stub

(@template ListOfBall)
(define (next-list lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-ball (first lob))
               (next-list (rest lob)))]))


(@htdf touch-top?)
(@signature Ball -> Boolean)
;; true if ball is going up and CENTER hits top edge of box
(check-expect (touch-top?    (make-ball 11 (+ TOP 5) 3 -4)) false)
(check-expect (touch-top?    (make-ball 11 (+ TOP 4) 3 -4)) true)
(check-expect (touch-top?    (make-ball 11 (+ TOP 1) 3 -2)) true)
(check-expect (touch-top?    (make-ball 11 (+ TOP 0) 3  2)) false)
#;
(define (touch-top? b) false)

(@template Ball)

(define (touch-top? b) 
  (<= (+ (ball-y b) (ball-dy b)) TOP))


(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; true if ball is going down and CENTER hits bottom edge of box
(check-expect (touch-bottom? (make-ball 11 (- BOT 3) 3  2)) false)
(check-expect (touch-bottom? (make-ball 11 (- BOT 2) 3  2)) true)
(check-expect (touch-bottom? (make-ball 11 (- BOT 0) 3  2)) true)
(check-expect (touch-bottom? (make-ball 11 (- BOT 0) 3 -2)) false)
#;
(define (touch-bottom? b) false)

(@template Ball)

(define (touch-bottom? b) 
  (>= (+ (ball-y b) (ball-dy b)) BOT))


(@htdf touch-left?)
(@signature Ball -> Boolean)
;; true if ball is going left and CENTER hits left  edge of box
(check-expect (touch-left?   (make-ball (+ LEF 6) 11 -5 2)) false)
(check-expect (touch-left?   (make-ball (+ LEF 5) 11 -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) 11 -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) 11  3 2)) false)
#;
(define (touch-left? b) false)

(@template Ball)

(define (touch-left? b) 
  (<= (+ (ball-x b) (ball-dx b)) LEF))


(@htdf touch-right?)
(@signature Ball -> Boolean)
;; true if ball is going right and CENTER hits right edge of box
(check-expect (touch-right?  (make-ball (- RIG 6) 11  5 2)) false)
(check-expect (touch-right?  (make-ball (- RIG 5) 11  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) 11  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) 11 -3 2)) false)
#;
(define (touch-right? b) false)

(@template Ball)

(define (touch-right? b)  
  (>= (+ (ball-x b) (ball-dx b)) RIG))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of top edge of the box
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-expect (bounce-top (make-ball 13  TOP 2 -3))
              (make-ball 13 (+ TOP 1) 2 3))
(check-expect (bounce-top (make-ball 14 (- TOP 1) 5 -6))
              (make-ball 14 (+ TOP 1) 5 6))
#;
(define (bounce-top b) b)

(@template Ball)

(define (bounce-top b) 
  (make-ball (ball-x b) (+ TOP 1) (ball-dx b) (- (ball-dy b))))


(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of bottom edge of the box
;; CONSTRAINT: assume ball is close to bottom edge and moving down
(check-expect (bounce-bottom (make-ball 13 BOT 2 3))
              (make-ball 13 (- BOT 1) 2 -3))
(check-expect (bounce-bottom (make-ball 14 (+ BOT 1) 5 6))
              (make-ball 14 (- BOT 1) 5 -6))
#;
(define (bounce-bottom b) b)

(@template Ball)

(define (bounce-bottom b)
  (make-ball (ball-x b) (- BOT 1) (ball-dx b) (- (ball-dy b))))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of left edge of the box
;; CONSTRAINT: assume ball is close to left edge and moving left
(check-expect (bounce-left (make-ball LEF  13 -3 2))
              (make-ball (+ LEF 1) 13 3 2))
(check-expect (bounce-left (make-ball (- LEF 1) 14 -6 5))
              (make-ball (+ LEF 1) 14 6 5))
#;
(define (bounce-left b) b)

(@template Ball)

(define (bounce-left b) 
  (make-ball (+ LEF 1) (ball-y b) (- (ball-dx b)) (ball-dy b) ))


(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball that has bounced 1 pixel off of right edge of the box
;; CONSTRAINT: assume ball is close to right edge and moving right
(check-expect (bounce-right (make-ball RIG 13 2 3))
              (make-ball (- RIG 1) 13 -2 3))
(check-expect (bounce-right (make-ball (+ RIG 1) 14 5 6))
              (make-ball (- RIG 1) 14 -5 6))
#;
(define (bounce-right b) b)

(@template Ball)

(define (bounce-right b)
  (make-ball (- RIG 1) (ball-y b) (- (ball-dx b)) (ball-dy b)))


(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
(check-expect (glide (make-ball 100 200 2 3)) (make-ball 102 203 2 3))
(check-expect (glide (make-ball 50 220 -3 -2)) (make-ball 47 218 -3 -2))
#;
(define (glide b) b)

(@template Ball)

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b))
             (+ (ball-y b) (ball-dy b))
             (ball-dx b)
             (ball-dy b)))


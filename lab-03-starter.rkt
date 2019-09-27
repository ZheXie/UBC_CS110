;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment lab-03)
(@cwl xiezhe12) 

; Balloon popping

(@htdw Balloon)
;; CONSTANTS ==========================

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define BALLOON-COLOR "red")
(define POP-IMAGE
  (overlay (text "POP!" 80 "black")
           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2) "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 2)

(define MAX-SIZE (/ WIDTH 2))





;; DATA DEFINITIONS ============================
 
(@problem 1)

(@htdd Balloon)
;; Balloon is one of
;; Natural 
;; false
;; interp. Natural is the radius of the growing balloon
;;         flase is the popped balloon 
;;CONSTRAINT: Natural should be in [1, MAX-SIZE]
(define B1 30)
(define B2 false)



(@dd-template-rules one-of              ;2 cases
                    atomic-non-distinct ;Natural 
                    atomic-distinct)    ;false


(define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else (...)]))



;; FUNCTIONS ====================================
(@problem 2)
(@htdf main)
(@signature Balloon -> Balloon)  
;; starts the world program with 1
; no examples for main function

(@template htdw-main)
(define (main b)
  (big-bang b                          ; Balloon
            (on-tick tick)     ; Balloon -> Balloon
            (to-draw render)))         ; Balloon -> Image


(@problem 3)
(@htdf tick)
(@signature Balloon -> Balloon) 
;; produce the next balloon by add SPEED to the current balloon,
;; produce false if the next balloon is bigger than MAX-SIZE after adding SPEED
(check-expect (tick 30) (+ 30 SPEED))
(check-expect (tick (- MAX-SIZE SPEED)) MAX-SIZE)
(check-expect (tick (- MAX-SIZE 1)) false)
(check-expect (tick MAX-SIZE) false)
(check-expect (tick (+ MAX-SIZE 1)) false)


;(define (tick b) false)

(@template Balloon)
(define (tick b)
  (cond [(and (number? b) (<= (+ b SPEED) MAX-SIZE)) (+ b SPEED)]
        [else false]))



(@problem 4)
(@htdf render)
(@signature Balloon -> Image) 
;; consume a Ballon and produce a ballon image on the CTR-X CTR-Y an MTS 
;;  if the Balloon is less than MAX-SIZE after adding SPEED, place the 
;;  POP-IMAGE at the same place if the Balloon is false 
(check-expect (render 30)
              (place-image (circle 30 "solid" BALLOON-COLOR)
                           CTR-X CTR-Y MTS))
(check-expect (render MAX-SIZE)
              (place-image (circle MAX-SIZE "solid" BALLOON-COLOR)
                           CTR-X CTR-Y MTS))
(check-expect (render false)
              (place-image POP-IMAGE CTR-X CTR-Y MTS))


;(define (render b) empty-image)


(@template Balloon)
 
(define (render b)
  (cond [(and (number? b) (<= 1 b MAX-SIZE))
         (place-image (circle b "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)]
        [else (place-image POP-IMAGE CTR-X CTR-Y MTS)]))

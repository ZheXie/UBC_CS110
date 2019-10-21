;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec07-lon-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 (cons 3 empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Number ListOfNumber)
                    self-ref)          ;(rest lon) is ListOfNumber

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))


#|
PROBLEMs:

Design a function that computes the sum of a list of numbers.

Design a function that counts the number of elements in a
list of numbers.

Design a function that produces a new list, where each element  
is 2 times the corresponding element in the original list.
|#

(@htdf sum)
(@signature ListOfNumber -> Number)
;; add every number in the list, if the list is empty, add zero
(check-expect (sum LON1) 0)
(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum LON2) (+ 1 2 3))

;(define (sum lon) 0)

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
           (sum (rest lon)))]))

(@htdf count) 
(@signature ListOfNumber -> Number)
;; count the number of numbers in the lon
(check-expect (count empty) 0)
(check-expect (count (cons 3 (cons 2 (cons 1 empty)))) 3)
(check-expect (count (cons 0 empty)) 1)

             
;(define (count lon) 0)

(define (count lon)
  (cond [(empty? lon) 0]
        [else
        (+ 1 (count (rest lon)))]))


(@htdf double)
(@signature ListOfNumber -> Number)
;; multiply the numbers in the lon by 2

(define
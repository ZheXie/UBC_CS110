;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec07-los-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd ListOfString)
;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "Canucks" empty))
(define LOS3 (cons "Leafs" (cons "Canucks" empty)))
(define LOS4 (cons "Canadiens" (cons "Leafs" (cons "Canucks" empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons String ListOfString)
                    self-ref)          ;(rest los) is ListOfString

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

#|
PROBLEM:

Design a function that determines whether "Canucks" appears in a 
list of strings.
|#

(require spd/tags)


(@htdd ListOfString)
;;ListOfString is (cons String empty)
;; - empty
;; - (cons String ListOfString)
;; interp. list of strings


(define LOS0 empty)
(define LOS1 (cons "Canucks" empty))
(define LOS2 (cons "Flyers" (cons "Sharks" empty)))
(define LOS3 (cons "Penguins" (cons "Leafs" (cons "Bruins" empty))))


(@dd-template-rules one-of           ;2 cases
                    atomic-distinct  ;empty
                    compound         ;(cons String ListOfString)
                    self-ref)        ;(rest los) is ListOfString

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (rest los))]))


(@htdf contains-heartbreak?)
(@signature ListOfString -> Boolean)
;; produce true if list contains "Canucks"
(check-expect (contains-heartbreak? empty) false)
(check-expect (contains-heartbreak? (cons "Canucks" empty)) true)
(check-expect (contains-heartbreak? (cons "Canadians" (cons "Canucks" empty)))
              true)
(check-expect (contains-heartbreak? (cons "Bruons" empty)) false)
(check-expect (contains-heartbreak? (cons "Canadians" (cons "Leafs" empty)))
                                    false)


;(defien (contains-heartbreak? los) false) ;stub

(@template ListOfString)

(define (contains-heartbreak? los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) "Canucks")
             true
             (contains-heartbreak? (rest los)))]))
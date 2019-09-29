;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pset-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
;;
(require spd/tags)
(require 2htdp/image)

(@assignment pset-02);Do not edit or remove this tag
(@cwl xiezhe12)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


(@problem 1)
;;
;; The following function design may have errors in it.  Please fix the error
;; or errors that you find.  Any changes you make should preserve the existing
;; design intent.  
;;
;; First uncomment the entire function design, and then fix the error.
;; If you are unable to find and fix the error, leave it commented out.
;;
;; Your solution must include @htdf, @signature, and @template tags.

(@htdf stack)
(@signature Image Image -> Image)
;; stack images, widest on bottom, first on bottom if same width
(check-expect (stack (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red")))
(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 20 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))
(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 10 9 "solid" "red"))
              (above (rectangle 10 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))

;(define (stack i1 i2) i1)   ;stub

(@template Image)

(define (stack i1 i2)
  (if (>= (image-width i1) (image-width i2))
      (above i2 i1)
      (above i1 i2)))





(@problem 2)
;;
;; The following data design may have errors in it. Please fix the error
;; or errors that you find.  Any changes you make should preserve the existing
;; design intent.
;;
;; Your solution must include @htdd tag, @dd-template-rules tag, and
;; a NOT commented fn-for-site template.
;; 


(@htdd Site)
;; Site is one of:
;; - "Vancouver"
;; - "Okanagan"
;; - "Robson"
;; - "CDM"
;; interp. a UBC site
;; <Examples are redundant for enumerations>

(@dd-template-rules one-of
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct)

(define (fn-for-site s)
  (cond [(string=? s "Vancouver") (...)]
        [(string=? s "Okanagan") (...)]
        [(string=? s "Robson") (...)]
        [(string=? s "CDM") (...)]))




(@problem 3)
;; Problems 3 and 4:
;; 
;; Consider the following data definition for Likert ratings.
;; 

(@htdd Rating)
;; Rating is one of:
;;  - "n/a"
;;  - Natural
;; interp. a Likert rating on a survey, either not applicable or rating
;;         1 is least happy, 5 is most happy, 3 is neither happy nor unhappy
;; CONSTRAINT: if a natural, is always in [1, 5]
(define R1 "n/a")  ;didn't vote
(define R2 5)      ;happiness with UBC!

(@dd-template-rules one-of
                    atomic-distinct
                    atomic-non-distinct)
#;
(define (fn-for-rating r)
  (cond [(and (string? r) (string=? r "n/a")) (...)] ;string=? not needed
        [else(... r)]))
         

;;
;; Ali wants to work with ratings, but first needs two functions.
;; The first consumes a rating and produces true if it represents a
;; real preference.  n/a and 3 not real preferences, 1/2/4/5 are.
;; The second function consumes a rating that is definitely a real
;; preference and produces true if the preference is positive.
;; Design both functions.  Call them preference? and happy?
;;

;put the design of preference? here
(@htdf preference?)
(@signature Rating -> Boolean)
;; consume a rating and produce ture if it represents a real preference.
(check-expect (preference? "n/a") false)
(check-expect (preference? 3) false)
(check-expect (preference? 5) true)
(check-expect (preference? 4) true)
(check-expect (preference? 2) true)
(check-expect (preference? 1) true)

; (define (preference? r) true)     ;stub

(@template Rating)
(define (preference? r)
  (cond [(and (string? r) (string=? "n/a" r)) false] 
        [else (or (> 6 r 3) (< 0 r 3))]))
         
         
(@problem 4) ;complete the design of happy? here
(@htdf happy?) 
(@signature Rating -> Boolean)
;; produce true if a real positive/negative happy
;; CONSTRAINT: r should not be "n/a" or 3

;; NOTE that the CONSTRAINT line above means that if happy? is
;; called with "n/a" or 3 it may produce a meaningless result.
;; This might be false, but no test should check for it since it
;; is meaningless.  This does NOT mean you should use a different
;; template for happy? Use the normal Rating template.

;; produce true if the given rating is greater than 3.
(check-expect (happy? "n/a") false)
(check-expect (happy? 3) false)
(check-expect (happy? 4) true)
(check-expect (happy? 2) false)
(check-expect (happy? 5) true)
(check-expect (happy? 1) false)

; (define (happy? r) false) ;stub

(@template Rating)
(define (happy? r)
   (cond [(and (string? r) (string=? r "n/a")) false] ;string=? not needed
        [else (< 3 r 6)])) 



(@problem 5)
;;
;; Write the dd-template-rules tag and template for the following
;; type comment.  Your rules and template must be in the same order
;; as the type comment.
;;

(@htdd Altitude)
;; Altitude is one of:
;;  - "pre-launch"
;;  - Number
;;  - "post-flight"
;; interp. Altitude of rocket. Before launch, in meters above launch
;;         pad, after flight has ended.
;; CONSTRAINT: when a number is > 0
(define A0 "pre-launch")
(define A1 37.5)
(define A2 "post-flight")

(@dd-template-rules one-of                ;3 cases
                    atomic-distinct       ;"pre-launch"
                    atomic-non-distinct   ;37.5
                    atomic-distinct)      ;"post-flight"

(define (fn-for-altitude a)
  (cond [(string=? a "pre-launch") (...)]
        [(and (number? a) (> a 0)) (... a)]
        [(string=? a "post-flight") (...)]))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec03-grades-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)


(@problem 1)
;;
;; Design a data definition to represent a grade on a 110 exam.
;; 
;; Then design a function that given a grade produces an appropriate comment 
;; that describes the grade. For example, grades 90 and above might warrant
;; "Excellent". 
;;

;; Data definitions:
(@htdd SportsTeamNames)
;SportsTeamName is string
;intrep. the string is the name of sports team names
;constrain
(define STN1 "T-Bird")
(define STN2 "Shit")
(@dd-template-rules atomic-non-distinct)  ;string
(define (fn-for-sports-team-names n) (... n))
  


(@htdf best?)
(@signature SportsTeamNames -> Boolean)
;produce "the best team in the world" if the sports team name is Shit
(check-expect (best? "T-Bird") "NO")
(check-expect (best? "Shit") "the best team in the world")


;(define (best? n) true)  ;stub

(@template SportsTeamNames)
(define (best? n) (if (string=? n "Shit")
                      "the best team in the world"
                      "NO"))
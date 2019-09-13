;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec03-teams-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)


(@problem 1)
;;
;; Design a data definition to represent the name of any sports team.
;;
;; Then design a function that consumes a team name and determines whether that 
;; is the best team in he world. You can use your own preferences to determine
;; what is the best team in the world.  (As long as it isn't the Yankees.)
;;

;; Data definitions
(@htdd ExamGrade)
;;ExamGrade is Natural
;;interp. the number is the percent grade
;;CONSTRAIN: <=100
(define EG1 100)
(define EG2 50)
(@dd-template-rules atomic-non-distinct) ;Natural
(define (fn-for-exam-grade g) (... g))


;; Functions
(@htdf comment)
(@signature ExamGrade -> String)
;; produce comment about grade
(check-expect (comment 100) "exllent")
(check-expect (comment 90) "exllent")
(check-expect (comment 89) "good")
(check-expect (comment 80) "good")
(check-expect (comment 79) "OK")
(check-expect (comment 0) "OK")


;(define (comment g) "")   ;stub
(@template ExamGrade)
(define (comment g) (cond [(>= g 90) "exllent"]
                          [(>= g 80) "good"]
                          [else "OK"]))

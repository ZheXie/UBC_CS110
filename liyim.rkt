;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname liyim) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;string->string                  this is signature
;add an s to every string        this is purpose

;(check-expect (pluralize "dog") "dogs")
;(check-expect (pluralize "liyiming") "liyimings")

;(define (pluralize str) " ")        ;this is stub
;(define (pluralize str)           ;this is template
;  (... str))

(define (pluralize str) (string-append str "s"))
(pluralize "liyim")
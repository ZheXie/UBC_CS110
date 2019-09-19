;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-htdf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment lab-02)
(@cwl xiezhe12)          ;!!! FILL IN YOUR CSID TO BE ABLE TO HANDIN

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)
(@htdf square?) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Boolean)
;; produce true if the height and width of the given image is same.
(check-expect (square? (rectangle 30 30 "solid" "blue")) true)
(check-expect (square? (rectangle 10 20 "solid" "red")) false)
(check-expect (square? (circle 20 "outline" "green")) true)


;; (define (square? img) false)     ;stub


(@template Image)
;; (define (square? img) (...img))

(define (square? img)
  (= (image-height img) (image-width img)))







;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
;; 
;; 
;; For example, 
;; 
;;   (underline (circle 20 "solid" "green"))
;; 
;; should produce
;;
;;   (above (circle 20 "solid" "green")
;;          (rectangle 40 2 "solid" "black"))
(@problem 2)
(@htdf underline) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Image)
;; consume a image and produce the image with an underline.
(check-expect (underline (rectangle 30 20 "solid" "yellow"))
              (above (rectangle 30 20 "solid" "yellow")
                     (rectangle 30 2 "solid" "black")))
(check-expect (underline (circle 20 "solid" "green"))
              (above (circle 20 "solid" "green")
                     (rectangle 40 2 "solid" "black")))


;; (define (underline img) empty-image)     ;stub

(@template Image)
;; (define (underline img) (... img))


(define (underline img)
  (above img (rectangle (image-width img) 2 "solid" "black")))












;; HtDF Lab, Problem 3

;; PROBLEM:
;; 
;; A (much too) simple scheme for pluralizing words in English is to add an 
;; s at the end unless the word already ends in s.
;; 
;; Design a function that consumes a string, and adds s to the end unless 
;; the string already ends in s.
(@problem 3)
(@htdf pluralize) ;!!!UNCOMMENT this line when you start on this function
(@signature String -> String)
;; add an s to a given word except the word end with the letter s
(check-expect (pluralize "dog") "dogs")
(check-expect (pluralize "cats") "cats")


;;(define (pluralize str) "")    ;stub

(@template String)
;; (define (pluralize str) (... str))

(define (pluralize str)
  (if (string=? (substring str (- (string-length str) 1)) "s")
      str
      (string-append str "s")))












;; HtDF Lab, Problem 4

;; PROBLEM:
;; 
;; Design a function called nth-char-equal? that consumes two strings 
;; and a natural and produces true if the strings both have length greater 
;; than n and have the same character at position n.
;; 
;; 
;; Note, the signature for such a function is:
;; 
;; (@signature String String Natural -> Boolean)
;; 
;; 
;; The tag and template for such a function are:
;; 
;; (@template String)
;; 
;; (define (nth-char-equal? s1 s2 n)
;;   (... s1 s2 n))
(@problem 4)
(@htdf nth-char-equal?) ;!!!UNCOMMENT this line when you start on this function
(@signature String String Natural -> Boolean)
;; consume two strings and a natural, produce ture if the strings
;; both have length greater than n and have the same character at position n
(check-expect (nth-char-equal? "aaaaaaaa" "aaaaaaaa" 4) true)
(check-expect (nth-char-equal? "aaaaa" "aaaaa" 4) true)
(check-expect (nth-char-equal? "aaaa" "aaaa" 4) false)
(check-expect (nth-char-equal? "aaaaa" "aaaab" 4) false)
(check-expect (nth-char-equal? "sa" "s" 0) true)
;; (define (nth-char-equal? s1 s2 n) false)   ;stub

(@template String)

;; (define (nth-char-equal? s1 s2 n)
;;    (... s1 s2 n))

(define (nth-char-equal? s1 s2 n)
     (cond [(or (<= (string-length s1) n) (<= (string-length s2) n)) false]
           [(and (= (string-length s1) (+ n 1)) (= (string-length s2) (+ n 1)))
            (string=? (substring s1 n) (substring s2 n))]
           [(and (> (string-length s1) (+ n 1)) (> (string-length s2) (+ n 1)))
            (string=? (substring s1 n (+ n 1)) (substring s2 n (+ n 1)))]
           [(and (> (string-length s1) (+ n 1)) (= (string-length s2) (+ n 1)))
            (string=? (substring s1 n (+ n 1)) (substring s2 n))]
           [(and (= (string-length s1) (+ n 1)) (> (string-length s2) (+ n 1)))
            (string=? (substring s1 n) (substring s2 n (+ n 1)))]))











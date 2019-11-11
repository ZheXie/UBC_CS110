;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lec13-2-one-of-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)


(define-struct terminal (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-terminal String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  terminal regions have label, weight and color
;;  groups just have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  given terminal contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions


(@problem 1)
#|
Design a function that consumes two lists of strings and produces
true if the first list is a prefix of the second, meaning that 
the first list matches, 1 for 1, the beginning of the second list.

For example:

  (prefix=? (list "a" "b") (list "a" "b" "c")) --> true
  (prefix=? (list "a" "b") (list "b" "c"))     --> false
  (prefix=? (list "a" "b") (list "a" "b"))     --> true
  (prefix=? (list "a" "b") (list "a"))         --> false

  
As a reminder, here is a data definition for a list of strings. To 
save space later we are calling it LOS instead of ListOfString.
|#

;; Data Definitions:

(@htdd LOS)
;; LOS is one of:
;;  - empty
;;  - (cons String LOS)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))

;; Functions:
(@htdf prefix=?)
(@signature LOS LOS -> Boolean)
;; produce true if L1 is a prefix of L2
(check-expect (prefix=? empty empty) true)
(check-expect (prefix=? empty (list "a")) true)
(check-expect (prefix=? (list "a") empty) false)

(check-expect (prefix=? (list "a") (list "a")) true)
(check-expect (prefix=? (list "a" "b") (list "a" "b" "c")) true)
(check-expect (prefix=? (list "a" "b") (list "b" "c")) false)
(check-expect (prefix=? (list "a" "b") (list "a" "b")) true)
(check-expect (prefix=? (list "a" "b") (list "b" "a")) flase)
(check-expect (prefix=? (list "a" "b") (list "a")) false)


(define (prefix=? Li L2) false)

(@template 2-one-of)
(define (prefix=? L1 L2)
  (cond [(empty? L1) true]
        [(empty? L2) false]
        [else
         (and (string=? (first L1) (first L2))
              (prefix=? (rest L1) (rest L2)))]))















;; ======================================================================

(@problem 2)
#|
Design a function that consumes two lists of numbers. Each list
is already sorted in increasing order. The function should produce
the merged list sorted in increasing order.
Show the cross product of type comments table, the simplification,
and the correspondence between table cells and cond cases.

For example:

  (merge (list 2 3 5) (list 1 4 6)) --> (list 1 2 3 4 5 6) 
  
As a reminder, here is a data definition for a list of numbers To 
save space later we are calling it LON instead of ListOfNumber
|#

;; Data Definitions:

(@htdd LON)
;; LON is one of:
;;  - empty
;;  - (cons Number LON)
;; interp. a list of numbers
(define LON1 empty)
(define LONA (list 2 3 5))
(define LONB (list 1 4 6))

;; Functions:
(@htdf 

















;; ======================================================================
;;
;; Consider the following two data definitions:
;;

(@htdd BinaryTree)

(define-struct node (k v l r))
;; BinaryTree is one of:
;;  - false
;;  - (make-node Natural String BinaryTree BinaryTree)
;; interp. 
;;  a binary tree, each node has a key, value and l/r children

(define BT0 false)
(define BT1 (make-node 1 "a" false false))
(define BT4 (make-node 4 "d"
                       (make-node 2 "b"
                                  (make-node 1 "a" false false)
                                  (make-node 3 "c" false false))
                       (make-node 5 "e" false false)))

(@htdd Path)
;; Path is one of:
;; - empty
;; - (cons "L" Path)
;; - (cons "R" Path)
;; interp. 
;;  A sequence of left and right 'turns' down through a BinaryTree
;;  (list "L" "R" "R") means take the left child of the tree, then
;;  the right child of that subtree, and the right child again.
;;  empty means you have arrived at the destination.

(define P1 empty)
(define P2 (list "L"))
(define P3 (list "R"))
(define P4 (list "L" "R"))

(@problem 3)
#|
Design the function has-path? that consumes BinaryTree and Path.
The function should produce true if following the path through the
tree leads to a node. If the path leads to false, or runs into false
before reaching the end of the path the function should produce false. 
Show the cross product of type comments table, the simplification,
and the correspondence between table cells and cond cases.
|#
  

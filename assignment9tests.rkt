#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")

;; add-nums
(display "add-nums\n")
(equal? (add-nums (list)) 0) ;;empty list
(equal? (add-nums (list 1 2 'a 3)) 6) ;; non-number

(equal? (add-nums (list 'c)) 0)
(equal? (add-nums (list 5)) 5)
;; length
(display "length\n")
(equal? (length (list)) 0) ;; empty list
(equal? (length (list 2)) 1) ;; len 1
(equal? (length (list 1 2 3 4)) 4)


;; get-nth
(display "get-nth\n")
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))   ;;negative index

(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list too short"))])
    (get-nth (list 1) 1))   ;;negative index

  (equal? (get-nth (list 2 3) 1) 3)


;; every-other
(display "every-other\n")
(equal? (every-other (list 1 2 3 4)) (list 1 3)) ;; even length
(equal? (every-other (list 1 2 3)) (list 1 3))   ;; odd length
(equal? (every-other (list 1)) (list 1))
(null? (every-other null))
;; map
(display "map\n")
(equal? (map (lambda (x) (* x x)) (list 1 2 3))
     (list 1 4 9))       ;; squaring
(null? (map (lambda (x) (* x x)) null))
(equal? (map (lambda (x) (+ 1 x)) (list 1)) (list 2))

;; map2
(display "map2\n")
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
     (list 2 6 12))      ;; multiply
(null? (map2 (lambda (x y) (* x y)) (list 1 1 1) null))
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 1 2)) (list 1 4))

;; filter
(display "filter\n")
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4))
     (list 1 3))      ;; odd
(null? (filter (lambda (x) #f) (list 1 2 3 4)))
(equal? (filter (lambda (x) #t) (list 1 2)) (list 1 2))
(null? (filter (lambda (x) #t) null))

;; call-all
(display "call-all\n")
(equal? (call-all (list (lambda () 2)))
     (list 2))        ;; one-element
(null? (call-all null))
(equal? (call-all (list (lambda () 3) (lambda () 4)))
      (list 3 4))
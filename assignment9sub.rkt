#lang racket
;; Programming Languages, Assignment 9
;;
;; This assignment is meant to help you catch up on the basic Racket syntax.
;;
;; Use `define` for all your function definitions. You can use either the `lambda`
;; version of define or the syntactic sugar'ed version.
(provide (all-defined-out))

;; Write a function `add-nums`. It takes as input a list and it adds up those elements
;; in the list that are actually numbers. You can check if something is a number via
;; `number?`. The result for an empty list should be 0.
;; The reference solution is 5 lines.

(define (add-nums lst)
  (if (null? lst)
      0
      (if (number? (car lst))
          (+ (car lst) (add-nums (cdr lst)))
          (add-nums (cdr lst)))))

;; Write a function `length`. It takes as input a list and returns the length of the
;; list.
;; The reference solution is 4 lines.

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

;; Write a function `get-nth`. It takes as input a list and an integer, and it returns
;; the n-th element in the list, starting at index 0. If the integer is negative it
;; should return `(error "negative index")`. If the list is not long enough it should
;; return `(error "list too short")`.
;; The reference solution is 5 lines.
(define (get-nth lst n)
  (if (< n 0)
      (error "negative index")
          (if (equal? n 0)
              (car lst)
              (if (null? lst)
                  (error "list too short")
                  (get-nth (cdr lst) (- n 1))))))

;; Write a function `every-other`. It takes as input a list, and it returns a new list
;; where every other term is skipped. So applied to the list `'(1 2 3)` it should return
;; `'(1 3)`, and the same for the list `'(1 2 3 4)`.
;; The reference solution is 5 lines.

(define (every-other lst)
  (cond [(null? lst) null]
        [(or (null? (cdr lst)) (null? (cdr (cdr lst))))
             (cons (car lst) null)]
        [#t (cons (car lst) (every-other (cdr (cdr lst))))]))


;; Write a function `map`. It takes two arguments: a function and a list. It then
;; returns a new list of the result of applying the function on each element.
;; The reference solution is 5 lines.

(define (map f lst)
  (cond [(null? lst) null]
        [(null? (cdr lst)) (cons (f (car lst)) null)]
        [#t (cons (f (car lst)) (map f (cdr lst)))]))

;; Write a function `map2`. It takes three arguments: a function that takes two inputs
;; and two lists. It then creates a single new list by applying the function to pairs
;; of values one from each list. The process stops when one of the lists is empty.
;; The reference solution is 5 lines.

(define (map2 f lst1 lst2)
  (cond [(or (null? lst1) (null? lst2)) null]
        [(or (null? (cdr lst1)) (null? (cdr lst2))) (cons (f (car lst1) (car lst2)) null)]
        [#t (cons (f (car lst1) (car lst2)) (map2 f (cdr lst1) (cdr lst2)))]))

;; Write a function `filter`. It takes as input a function and a list and returns
;; a new list consisting of those elements for which the function does not return #f
;; The reference solution is 5 lines.

(define (filter f lst)
  lst)

;; Write a function `call-all`. It takes as input a list of "thunks", and returns a
;; list of the results of calling those thunks. To call a function, you put it as the
;; first entry in parentheses, followed by any arguments it may have.
;; The reference solution is 4 lines.

(define (call-all th_lst)
  th_lst)
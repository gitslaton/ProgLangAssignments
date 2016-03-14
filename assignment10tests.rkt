#lang racket
;; Programming Languages, Assignment 10 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment10sub.rkt")

;; bind
(displayln "bind tests")
(define sample-env1 (list (binding 'x (num 4)) (binding 'y (num 5))))
(define sample-env2 (list (binding 'x (num 4)) (binding 'x (num 5))))

(equal? (bind 'x (num 4) empty) (list (binding 'x (num 4))))
(equal? (bind 'x (num 4) (bind 'y (num 5) empty)) sample-env1)
(equal? (bind 'x (num 4) (bind 'x (num 5) empty)) sample-env2)

;; lookup
(displayln "lookup tests")
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (lookup 'x empty)) 
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'x sample-env1) (num 4)))
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (lookup 'y sample-env1) (num 5)))
(with-handlers ([exn:fail? (lambda (exn) #t)])
  (equal? (lookup 'z sample-env1) (num 4)))


;; valid-program?
(displayln "valid-program? tests")
(define example1
  (call (fun #f 'x
             (if-e (isnul (var 'x))
                   (nul)
                   (arith '+ (fst (var 'x)) (snd (var 'x)))))
        (pair-e (num 5) (num 6))))
             
(valid-program? (num 5))
(not (valid-program? (num "f")))
(valid-program? example1)

;; value?
(displayln "value? tests")
(value? (num 5))
(value? (bool #t))
(not (value? (pair-e (arith '+ (num 2) (num 3)) (num 2))))

;; value-eq?
(displayln "value-eq? tests")
(value-eq? (num 5) (num 5))
(not (value-eq? (num 5) (bool #t)))
(not (value-eq? (num 5) (num 3.2)))

;; interp / evaluate
(displayln "interp/evaluate tests")
(equal? (evaluate (num 3))
        (num 3))
(equal? (evaluate (arith '* (num 3) (num 2)))
        (num 6))
;; We are using a try-catch form here because comp isn't
;; implemented yet. Your tests probably don't need to do that.
;; Similarly for subsequent uses of with-handlers.
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (comp '< (num 3) (num 2)))
          (bool #f)))


;; neq
(displayln "neq tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (neq (arith '+ (num 2) (num 3))
                         (num 6)))
          (bool #t)))

;; or2
(displayln "or2 tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #t)))


;; and2
(displayln "and2 tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and2 (comp '> (num 2) (num 3))
                         (bool #t)))
          (bool #f)))


;; or-e
(displayln "or-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (or-e (comp '> (num 2) (num 3))
                          (bool #f)
                          (bool #t)))
          (bool #t)))


;; and-e
(displayln "and-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate (and-e (comp '> (num 2) (num 3))
                           (bool #f)
                           (bool #t)))
          (bool #f)))


;; let-e*
(displayln "let-e* tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (let-e* (['x (num 2)]
                    ['y (arith '+ (var 'x) (num 4))])
                   (arith '+ (var 'x) (var 'y))))
          (num 8)))


;; plus / times
(displayln "plus/times tests")
;; Test commented out until you implement plus

;(equal? (evaluate (plus (num 2) (num 5) (num 3)))
;        (num 10))
;(equal? (evaluate (mult (num 2) (num 5) (num 3)))
;        (num 30))


;; minus
;(displayln "minus tests")
;(equal? (evaluate (minus (num 10) (num 5) (num 3)))
;        (num 2))


;; racketlist->sourcelist
(displayln "racketlist->sourcelist tests")
(equal? (racketlist->sourcelist (list (num 2) (num 5)))
        (pair-e (num 2) (pair-e (num 5) (nul))))

;; map-e
(displayln "map-e tests")
(with-handlers ([exn:fail? (lambda (exn) #f)])
  (equal? (evaluate
           (call (call map-e (fun #f 'x (plus2 (var 'x) (num 2))))
                 (pair-e (num 2) (pair-e (num 5) (nul)))))
          (pair-e (num 4) (pair-e (num 7) (nul)))))
         


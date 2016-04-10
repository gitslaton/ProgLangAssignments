#lang racket
;; Programming Languages, Assignment 10
;;
;; In this assignment we will build an interpreter in Racket. This will be a bit different from what we did
;; in OCAML:
;;
;; - There will be no parser step
;; - The language constructs are defined via structs
;; - Expressions in the source language are valid Racket expressions, written by directly using the structs.
;; - Desugaring can be thought of as Racket functions or macros that produce source language expressions.
;;
;; Most functions are provided default mock implementations. You will need to
;; correct them.
;;
;; Unlike previous assignment, you should feel free to search through the
;; Racket documentation if you need a specific function we have not seen
;; yet. But don't overdo it, there aren't many functions you need.
;;
;;
;; The things you need to implement have the word TODO in front of them.
(provide (all-defined-out))

;;           ENVIRONMENTS
;; We start by defining environments. Bindings are a struct, so we get 
;; functions: binding, binding?, binding-s, binding-v
(struct binding (s v) #:transparent)
;; An environment will be a list of "binding" structs
;; We start with an empty environment
(define empty null)
;;
;; TODO: Implement the function bind, which is given a symbol and a value
;; and an environment and it returns a new environment where a new binding
;; has been added to the front of the list binding s to v.
;; This should be a simple consing of a binding.
;;
(define (bind s v env)
  (cons (binding s v) env))       ;   <----- Need to implement this

;;
;; TODO: Implement the function lookup, that looks for the symbol s in
;; the structs contained in the list env, and returns the corresponding 
;; value v stored in the binding.
;; It should throw an appropriate "lookup failed" error if it can't find
;; the symbol.
(define (lookup s env)
  (if (null? env)
      (error (string-append "lookup: symbol not defined: "
                        (symbol->string s)))
    (if (equal? s (binding-s (car env)))
      (binding-v (car env))
      (lookup s (cdr env)))))
   

;;            THE LANGUAGE
;; We define the language in terms of structs.
;; We will treat the bool and num structs as both expressions and values
;; So your interpreter should be producing num or bool structs (or closures)
(struct var (s) #:transparent)
(struct num (n) #:transparent)
(struct bool (b) #:transparent)
(struct arith (op e1 e2) #:transparent)    ; arithmetic ops: +, -, *, /
(struct comp (op e1 e2) #:transparent)     ; comparison ops: <, <=, >, >=
(struct if-e (tst thn els) #:transparent)  ; conditional
(struct eq-e (e1 e2) #:transparent)          ; equality
(struct let-e (s e1 e2) #:transparent)     ; let s = e1 in e2
(struct fun (name arg body) #:transparent) ; functions
(struct call (e1 e2) #:transparent)        ; function call
(struct nul () #:transparent)              ; null
(struct isnul (e) #:transparent)           ; null?
(struct pair-e (e1 e2) #:transparent)      ; pair/cons-cell
(struct fst (e) #:transparent)             ; car
(struct snd (e) #:transparent)             ; cdr
;; The closures are not something that should show up on user programs
;; But it's a kind of value produced by interpreting programs.
(struct clos (f env))                     ; closure function environment

;; TODO: Write a function "valid-program?" that takes as input an
;; expression e and returns a Racket boolean on whether that expression
;; is a syntactically valid program. This is the analog of our program
;; passing the parser.
;;
;; The syntax rules are the following:
;;
;; The arith case is done for you as an example.
;; You will need to add many more cases to the cond.
(define (valid-program? e)
  (cond [(var? e)            ;; - A `var` is valid if `s` is a Racket symbol. Use `symbol?`
         (symbol? (var-s e))]
        [(num? e)            ;; - A `num` is valid if `n` is a Racket number. Use `number?`
         (number? (num-n e))]
        [(bool? e)           ;; - A `bool` is valid if `b` is a Racket boolean`. Use `boolean?`
         (boolean? (bool-b e))]
        [(arith? e)          ;; - A `arith` is valid if `op` is from the list '+, '-, '*, '/ and the `e1` and `e2` components are themselves valid
         (and (memq (arith-op e) (list '+ '* '- '/))
              (valid-program? (arith-e1 e))
              (valid-program? (arith-e2 e)))]
        [(comp? e)           ;; - A `comp` is valid if `op` is from the list '<, '<=, '>=, '> and the `e1` and `e2` components are themselves valid
         (and (memq (comp-op e) (list '< '<= '>= '>))
              (valid-program? (comp-e1 e))
              (valid-program? (comp-e2 e)))]
        [(if-e? e)           ;; - A `if-e` is valid if `tst`, `thn` and `els` are all valid
         (and (valid-program? (if-e-tst e))
              (valid-program? (if-e-thn e))
              (valid-program? (if-e-els e)))]
        [(eq-e? e)           ;; - A `eq-e` is valid if both `e1` and `e2` are valid
         (and (valid-program? (eq-e-e1 e))
              (valid-program? (eq-e-e2 e)))]
        [(let-e? e)          ;; - A `let-e` is valid if the `s` is an Racket symbol, and the `e1` and `e2` are themselves valid
         (and (symbol? (let-e-s e))
              (valid-program? (let-e-e1 e))
              (valid-program? (let-e-e2 e)))]
        [(fun? e)           ;; - A `fun` is valid if `arg` is a Racket symbol, and `name` is either #f or a Racket symbol distinct from `arg`, and the `body` is valid. `name` is used for recursively calling the function, if it is not #f.
         (and (symbol? (fun-arg e))
              (or (equal? #f (fun-name e))
                  (and (symbol? (fun-name e))
                       (not (equal? (fun-arg e) (fun-name e)))))
              (valid-program? (fun-body e)))]
        [(call? e)          ;; - A `call` is valid if both `e1` and `e2` are valid
         (and (valid-program? (call-e1 e))
              (valid-program? (call-e2 e)))]
        [(nul? e) #t]          ;; - A `nul` is valid
        [(isnul? e)           ;; - A `isnul` is valid if the `e` is valid
         (valid-program? (isnul-e e))]
        [(pair-e? e)        ;; - A `pair-e` is valid if both `e1` and `e2` are valid
         (and (valid-program? (pair-e-e1 e))
              (valid-program? (pair-e-e2 e)))]
        [(fst? e)           ;; - A `fst` is valid if `e` is valid
         (valid-program? (fst-e e))]
        [(snd? e)          ;; - A `snd` is valid if `e` is valid
         (valid-program? (snd-e e))]
        [else #f]))         ;; - Nothing else is valid (including closures)
 


;;     VALUES
;; We will use some of the structs above as values.
;;
;; TODO: Implement the function `value?` that returns if the expression
;; is a value. You do not need to worry about checking syntax errors at
;; this point. The rules for what is a value follow:
;;
;; - A `num` is a value
;; - A `bool` is a value
;; - A `nul` is a value
;; - A `clos` is a value
;; - A `pair-e` is a value only if both `e1` and `e2` are values
;;
;; Do this as a big "or". The first step is done for you, you need to
;; add the rest.
(define (value? e)
  (or (num? e)
      (bool? e)
      (nul? e)
      (clos? e)
      (and (pair-e? e)
           (and (value? (pair-e-e1 e))
                (value? (pair-e-e2 e))))))


;; TODO: Write a function `value-eq?` to test if two values are "equal".
;; Two values `v1`, `v2` are considered equal in the following cases:
;;
;; - They are both `num`s and the corresponding Racket numbers are equal.
;; - They are both `bool`s and the corresponding Racket bools are equal.
;; - They are both `nul`s.
;; - They are both `pair`s and their corresponding components are equal.
;; - Nothing else is equal (e.g. two closures cannot equal each other)
(define (value-eq? v1 v2)
  (cond [(and (num? v1) (num? v2))
         (equal? (num-n v1) (num-n v2))]
        [(and (bool? v1) (bool? v2))
         (equal? (bool-b v1) (bool-b v2))]
        [(and (nul? v1) (nul? v2)) #t]
        [(and (pair-e? v1) (pair-e? v2))
         (and (value-eq? (pair-e-e1 v1) (pair-e-e1 v2))
              (value-eq? (pair-e-e2 v1) (pair-e-e2 v2)))]
        [else #f]))         
              

;;       INTERPRETATION
;; We will now implement the interpreter. You may assume that what you are
;; given is a valid-program (no "syntax errors"). You will still need to
;; throw "implementation errors" if you are for instance asked to pick the
;; `fst` of something that is not a `pair-e`. The rules for interpretation
;; are as follows:
;;
;; - `num`, and `bool` and `nul` evaluate to themselves
;; - You do not need to check for a `clos` value as it's not allowed to
;;     be part of a valid program.
;; - A `var` needs to look the symbol up in the environment that is the
;;     argument to the interpreter function
;; - A `arith` needs to perform the correct operation based on the operator
;;     to the results of recursively interpreting e1, e2. It should throw
;;     an error if the e1, e2 don't evaluate to `num`s. This case is 
;;     provided for you. Note how we return a `num`, not a Racket number.
;; - A `comp` needs to perform the correct operation based on the operator
;;     to the results of recursively interpreting e1, e2. It should throw
;;     an error if the `e1, e2 don't evaluate to `num`s. You should return
;;     a `bool`.
;; - A `if-e` needs to first evaluate the `tst`. Throw an error if that is
;;     not a bool. Otherwise evaluate the appropriate `thn` or `els` depending
;;     on the bool's value.
;; - A `eq-e` needs to evaluate the two expressions and test them for equality
;;     using `value-eq?`
;; - A `let-e` needs to evaluate `e1` in the current environment, then extend
;;     the current environment by binding the symbol s to that value, and
;;     evaluate `e2` in this extended environment.
;; - A `fun` needs to create a `clos`.
;; - A `call` needs to evaluate `e1` and `e2`. Throw an error if `e1`
;;     did not evaluate to a `clos`. If it did evaluate to a `clos`, then
;;     you must perform that function call correctly, interpreting the
;;     closure's function's body in the appropriate environment.
;;     Note that one of the components of a `fun` element is `name`. 
;;     If it is set to #f then you can ignore it. But if it is not, then
;;     it is meant to be the name that the function can use to recursively
;;     call itself, so you need to bind it to the closure before you evaluate
;;     the body.
;;     This is probably the most complicated part of the interpreter.
;; - A `isnul` needs to evaluate the expression it contains, and returns a
;;     `bool` depending on whether that expression evaluated to `nul`.
;;     Make sure you understand the crucial difference between `isnul` and `nul?`.
;; - A `pair-e` evaluates to a `pair-e` containing the results of interpreting
;;     its components `e1`, `e2`.
;; - A `fst` evaluates the expression it contains. Throw an error if that
;;     is not a `pair-e`. If it is a `pair-e` then return its first component.
;; - A `snd` does the same as `fst`, but returning the second component instead.
;;
;; TODO: Write the function `interp` that takes an environment and an expression
;; and interprets the expression in the environment based on the above
;; description.
;; Some cases done for you.
(define (interp env e)
  (cond [(nul? e) nul] ;; eval to self
        [(num? e) e]   ;; eval to self
        [(arith? e)    ;; perform op on e1 and e2. Make sure e1 and e2 eval to nums.
         (let ([v1 (interp env (arith-e1 e))]
               [v2 (interp env (arith-e2 e))]
               [op (case (arith-op e)
                     ['+ +] ['- -] ['* *] ['/ /])])
           (if (and (num? v1) (num? v2))
               (num (op (num-n v1) (num-n v2)))
               (error "interp: arithmetic on non-numbers")))]
        [(bool? e) e] ;; eval to self
        [(var? e) (lookup (var-s e) env)] ;; get the val of the var from the environment
        [(comp? e)    ;; perform correct op on e1 and e2. Make sure both are nums.
         (let ([v1 (interp env (comp-e1 e))]
               [v2 (interp env (comp-e2 e))]
               [op (case (comp-op e)
                     ['> >] ['>= >=] ['< <] ['<= <=])])
         (if (and (num? v1) (num? v2))   
             (bool (op (num-n v1) (num-n v2)))
             (error "interp: comparison on non-numbers")))]
        [(if-e? e)   ;; Make sure tst evals to a bool. Perform correct action based on bool. Throw error if tst not bool.
         (if (bool? (interp env (if-e-tst e)))
             (let ([if_test (bool-b (interp env (if-e-tst e)))])
               (if if_test
                   (interp env (if-e-thn e))
                   (interp env (if-e-els e))))
             (error "interp: not a boolean test in if"))]
        [(eq-e? e)
         (bool (value-eq? (interp env (eq-e-e1 e)) (interp env (eq-e-e2 e))))]
        [(let-e? e)
         (let ([v1 (interp env (let-e-e1 e))])
           (interp (bind (let-e-s e) v1 env) (let-e-e2 e)))]
        [(fun? e)
         (clos (e env))]
        [(call? e)
         (let ([c (interp env (call-e1 e))])
          (if (clos? c)
            (if (equal? (fun-name (clos-f c)) #f)
               (interp (clos-env c) (call-e2 e))
               (interp (bind (fun-name (clos-f c)) (interp env (clos-f c)) (clos-env c)) (call-e2 e)))
            (error "interp: call on non-closure")))]
        [(isnul? e)
         (bool (nul? (interp env isnul-e e)))]
        [(pair-e? e)
         (pair-e (interp env (pair-e-e1 e)) (interp env (pair-e-e2 e)))]
        [(fst? e)
         (let ([p (interp env (fst-e e))])
           (if (pair-e? p)
               (interp env (pair-e-e1 p))
               (error "interp: cannot get first from non-pair")))]
        [(snd? e)
         (let ([p (interp env (snd-e e))])
           (if (pair-e? p)
               (interp env (pair-e-e2 p))
               (error "interp: cannot get second from non-pair")))]
        [else (error "interp: unknown expression")]))

;;         EVALUATE
;; This method simply calls the interpreter with an initially empty environment
;; Do not change it
(define (evaluate e)
  (interp empty e))


;;        DESUGARING   /   SURFACE LANGUAGE
;; We will create Racket functions and macros that act as desugaring
;; These should not perform any evaluation/interpretation, they merely
;; "rewrite" a "program" into a new "program".
;;
;; As an example, here is a Racket function `not-e` that performs a `not`:
(define (not-e e) (if-e e (bool #f) (bool #t)))

;; Basic examples for operations. You can use them to write
;; source language programs for testing.
(define (plus2 e1 e2) (arith '+ e1 e2))
(define (minus2 e1 e2) (arith '- e1 e2))
(define (mult2 e1 e2) (arith '* e1 e2))
(define (div2 e1 e2) (arith '/ e1 e2))
(define (leq e1 e2) (comp '<= e1 e2))
(define (le e1 e2) (comp '< e1 e2))
(define (geq e1 e2) (comp '>= e1 e2))
(define (ge e1 e2) (comp '> e1 e2))

;; TODO: Write a function `neq` that takes as input two source language
;; expressions and returns the expression that tests that they are not
;; equal. This should be a combination of `not-e` and `eq-e`.
(define (neq e1 e2)
  (not-e (eq-e e1 e2)))

;; TODO: Write a function `or2` that takes as input two source language
;; expressions `e1` and `e2` and returns the appropriate `if-e` expression
;; that performs the "or" of the two expressions.
(define (or2 e1 e2)
  (if-e e1 (bool #t) (if-e e2 (bool #t) (bool #f))))   

;; TODO: Write a function `and2` that takes as input two source language
;; expressions `e1` and `e2` and returns the appropriate `if-e` expression
;; that performs the "and" of the two expressions.
(define (and2 e1 e2)
  (if-e e1 (if-e e2 (bool #t) (bool #f)) (bool #f))) 

;; TODO: Write a function `or-e` that takes as input any number of source
;; language expressions as input and creates the corresponding nested
;; `if-e` expression that behaves like the "or" of those expressions.
;; If we define a function as `(lambda es body)`, i.e. no parentheses at
;; the second argument, then the function accepts any number of arguments
;; and puts them all in a list, named `es` in the above example.
;; A call to `(or-e)` with no arguments would thus end up in a call
;; where `es` is the null list. That call should be returning the false
;; bool struct.
;; You can do this with a call to `foldr`. Look at the documentation to
;; learn about the syntax for `foldr`.
(define or-e
  (lambda es
    (foldr or2 '() es)))      ; <------ Need to fix this

;; TODO: We will similarly do something for `and-e`, but for this one
;; we will instead build a macro. For no arguments, this should return
;; the language bool for true.
;; The two base cases are done for you.
(define-syntax and-e
  (syntax-rules ()
    [(and-e) (bool #t)]
    [(and-e e1) e1]
    [(and-e e1 e2 ...) (and2 e1 (and-e e2 ... ))]))   ; <--  Use and2 and "recursion"

;; TODO: Build a `let-e*` macro that takes input like:
;; `(let-e* ([s1 e1] [s2 e2] ...) e)` and creates the equivalent nested 
;; `let-e` expression.
;; The two base cases are done for you.
(define-syntax let-e*
  (syntax-rules ()
    [(let-e* () e) e]
    [(let-e* ([s1 e1]) e) (let-e s1 e1 e)]
    [(let-e* ([s1 e1] rest ...) e) (let-e s1 e1 (let-e* (rest ...) e))]))  ; <-- Need to fix this.

;; TODO: Write functions or macros `plus`, and `mult` that take any number
;; of source language expressions as arguments and creates a corresponding
;; nested `plus2` or `mult2` expression that computes the corresponding
;; sum or product. For zero arguments provided it should return the `num`
;; for 0 or 1 respectively.
;; You can choose either a macro approach like in `and-e` or a function
;; approach and `foldr` like in `or-e`.
(define-syntax plus
  (syntax-rules ()
    [(plus) (num 0)]
    [(plus e1 e2) (plus2 e1 e2)]
    [(plus e1 e2 ...) (plus2 e1 (plus e2 ... ))])) 

(define-syntax mult
  (syntax-rules ()
    [(mult) (num 1)]
    [(mult e1 e2) (mult2 e1 e2)]
    [(mult e1 e2 ...) (mult2 e1 (mult e2 ... ))])) 


;; TODO: Write a macro `minus` that takes one or more arguments and behaves
;; as follows:
;; - With one argument, it returns its negation (as a source language expression).
;; - With two or more arguments, it subtracts all the remaining arguments
;;    from the first one.
;; Try out the function `-` in Racket to see examples of the behavior.
;; Do this as a macro, similar to `and-e`.

(define-syntax minus
  (syntax-rules ()
    [(minus) (num 0)]
    [(minus e1) (minus2 (num 0) e1)]
    [(minus e1 e2 ...) (minus2 e1 (minus e2 ... ))])) 


;;            LISTS
;; A source language list is, analogously to Racket's lists, a nested
;; sequence of `pair-e` expressions where the `fst` (car) components
;; contain the values and the `snd` (cdr) components contain the
;; tails of the list. The list terminates when the tail is the `nul`.
;; An example would be: `(pair-e (num 2) (pair-e (num 5) (nul)))`
;;
;; TODO: Write a function `racketlist->sourcelist` that takes as input
;; a Racket list of (presumably source language) expressions and constructs
;; the corresponding source language list expression containing those
;; entries. For example `(racketlist->sourcelist (list (num 2) (num 5)))`
;; should return the example mentioned above.
;; Do this as a function that uses `foldr`.
(define racketlist->sourcelist
  (lambda (exps)
    #f))        ;  <--- Replace this with an appropriate foldr call.

;; TODO: Write a source language expression `map-e`. It should be a 
;; `fun` that takes as input a "fun" `f` and returns a `fun` that takes
;; as input a source language list `lst` and performs a "map" of the
;; function f on the list lst (when interpreted that is).
;; The first few lines, that set up the curried function, are done for you.
(define map-e
  (fun 'map 'f 
       (fun 'inner 'lst
            (nul))))   ; <--- Need to change this

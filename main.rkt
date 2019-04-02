#lang racket
; Racket interpreter - CPSC 3740 Final Project
; Nathan Tipper & Vincent Cote
; April 1st, 2019
(require "operators.rkt")
(require "lists.rkt")

; Helper function to add a new binding to the current list of bindings
; Parameter newBinding: the new binding to be added to the list
; Parameter currentBindings: the current ongoing list of bindings
(define (addBinding newBinding currentBindings)
  (cond
    [(symbol? (car newBinding)) (append (list newBinding) currentBindings)]
    [else (append (list (cdr newBinding)) currentBindings)])
  )

; Helper function to add a multiple new binding to the current list of bindings
; Parameter newBindings: the new bindings to be added to the list
; Parameter currentBindings: the current ongoing list of bindings
(define (addBindings newBindings currentBindings)
  (cond
    [(null? newBindings) currentBindings]
    [else (addBindings (cdr newBindings) (append (list (car newBindings)) currentBindings))]
  ))

; Helper function to bind lambda variables to the current bindings
; Parameter variable: the variables to add
; Parameter values: the values to the variables
; Parameter cbindings: the current ongoing list of bindings
(define (bindLambdaVariables variables values cbindings)
  (cond
    [(null? variables) '()]
    [(and (list? (car values)) (equal? (car variables) (car (member (car variables) (car values))))) (append (bindLambdaVariables (cdr variables) (cdr values) cbindings) (list (list (car variables) (myEval (car values) cbindings))))] 
    [(append (bindLambdaVariables (cdr variables) (cdr values) cbindings) (list (list (car variables) (car values))))])
  )

; Function to search the list of bindings to see if it is in the list
; Parameter binding: the binding to search for
; Parameter bindings: the current ongoing list of bindings
; Returns true if binding is in the list, false otherwise
(define (findBinding binding bindings)
  (cond
    [(null? bindings) #f]
    [(equal? binding (caar bindings)) #t]
    [(and (list? binding) (equal? (car binding) (caar bindings))) #t]
    [else (findBinding binding (cdr bindings))])
  )

; Function to return a binding from the list
; Parameter binding: the binding to to get
; Parameter bindings: the current ongoing list of bindings
; Returns the binding from the list
(define (getBinding binding bindings)
  (cond 
    [(and (list? binding) (equal? (car binding) (caar bindings)) (equal? (caadar bindings) 'lambda)) (createLambdaBindings (cdr binding) (list (cadar bindings)))]
    [(and (list? binding) (equal? (car binding) (caar bindings)) (equal? (car (caadar bindings)) 'lambda)) (createLambdaBindings (cdr binding) (list (caadar bindings)))]
    [(equal? binding (caar bindings)) (cadar bindings)]
    [else (getBinding binding (cdr bindings))]
  )
  )

; Function to append a value to a lambda from the bindings list
; Parameter binding: the value to be appended to the lambda
; Parameter lambda: the lambda expression to be appended to
(define (createLambdaBindings bindings lambda)
  (cond
    [(null? bindings) lambda]
    [else (createLambdaBindings (cdr bindings) (append lambda (list (car bindings))))]
    )
  )

; Helper function for a lambda in a list
; Parameter list: the list to search for lambda
; Returns true if there is a lambda, false otherwise
(define (isLambda list)
  (cond
    [(not (list? list)) #f]
    [(equal? (car list) 'lambda) #t]
    [else (isLambda (car list))]
    ))

(define (startEval program)
  (myEval program '())
  )

(define (myEval program bindings)
  (cond
    ; If the program is just an empty list, say fuck it and return 0
    [(equal? program '()) 0]
    [(findBinding program bindings) (myEval (getBinding program bindings) bindings)]
    ; If the program is not a list, just return whatever it is
    [(not (list? program)) program]
    ; If program is a list of only one list, evaulate the inner list
    [(and (list? program) (null? (cdr program))) (myEval (car program) bindings)]
    ; Get a variable
    ;[(findBinding (car program) bindings) (myEval (getBinding (car program) bindings) bindings)]
    ; Process any defines
    [(and (list? (car program)) (equal? (car (car program)) 'define)) (myEval (cdr program) (addBinding (cdr (car program)) bindings))]
    ; Process lambdas
    [(and (list? (car program)) (equal? (car (car program)) 'lambda)) (myEval (cdr (cdr (car program))) (append (bindLambdaVariables (car (cdr (car program))) (cdr program) bindings) bindings))]
    ; If the first element in the program is a list and the rest of the program is empty, evaluate the list in the program. Seems like a repeat of above but better... lawl
    [(and (list? (car program)) (null? (cdr program))) (myEval (car program) bindings)]
    ; If program wants to quote, ignore that crap
    [(equal? (car program) 'quote) (myEval (cdr program) bindings)]
    ; Add
    [(equal? (car program) '+) (add (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Subtract
    [(equal? (car program) '-) (sub (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Multiply
    [(equal? (car program) '*) (mult (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Division
    [(equal? (car program) '/) (div (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test equality
    [(equal? (car program) '=) (isEqual? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test greater than
    [(equal? (car program) '>) (isGreaterThan? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test less than
    [(equal? (car program) '<) (isLessThan? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test less than or equal to
    [(equal? (car program) '<=) (isLessOrEqualThan? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test greater than or equal
    [(equal? (car program) '>=) (isGreaterOrEqualThan? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Test object equality
    [(equal? (car program) 'equal?) (areEqual? (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings))]
    ; Return remainder of list
    [(equal? (car program) 'car) (myEval (getCar (cdr program)) bindings)]
    ; Return beginning of list
    [(equal? (car program) 'cdr) (myEval (getCdr (cdr program)) bindings)]
    ; Make a pair out of the two arguments
    [(equal? (car program) 'cons)(cons (myEval (car (cdr program)) bindings) (myEval (cdr (cdr program)) bindings))]
    ; Test to see if the program is a pair
    [(equal? (car program) 'pair?) (isPair? (myEval (cdr program) bindings))]
    ; Test an if condition
    [(equal? (car program) 'if) (if (myEval (car (cdr program)) bindings) (myEval (car (cdr (cdr program))) bindings) (myEval (cdr (cdr (cdr program))) bindings))]
    ; Test lambda variables
    ;[(equal? (car program) 'lambda) (myEval (car (cdr (cdr program))) bindings)]
    ; Test for let
    [(equal? (car program) 'let) (myEval (cdr (cdr program)) (addBindings (car (cdr program)) bindings))]
    ; Test for letrec
    [(equal? (car program) 'letrec) (myEval (cdr (cdr program)) (addBinding (car (car (cdr program))) bindings))]
    [else program])
  )

; TEST BED
(startEval '()) ; Empty - Expected Output: 0
(startEval '(2)) ; Number - Expected Output: 2
(startEval '((define x 5) (define y (+ 3 4)) (* x y))) ; Variables - Expected Output: 35
(startEval '(quote 4)) ; Quote - Expected Output: 4
(startEval '(+ (+ 7 3) (+ 2 3))) ; Addition - Expected Output: 15
(startEval '(- (- 7 3) (- 2 3))) ; Subtraction - Expected Output: -1
(startEval '(* (* 7 3) (* 2 3))) ; Multiplication - Expected Output: 105
(startEval '(/ (/ 30 3) (/ 25 5))) ; Division - Expected Output: 2
(startEval '(= (+ 3 3) 5)) ; Equality - Expected Output: False
(startEval '(= (/ 20 5) 4)) ; Equality - Expected Output: True
(startEval '(> 6 (* 3 2))) ; Greater Than - Expected Output: False
(startEval '(> (* 5 4) 5)) ; Greater Than - Expected Output: True
(startEval '(< 20 (* 5 4))) ; Greater Than - Expected Output: False
(startEval '(< (* 5 3) 20)) ; Greater Than - Expected Output: True
(startEval '(>= 7 (* 3 3))) ; Greatern Than - Expected Output: False
(startEval '(>= 9 (* 3 3))) ; Greater Than - Expected Output: True
(startEval '(<= 10 (* 3 3))) ; Greater Than - Expected Output: False
(startEval '(<= 9 (* 3 3))) ; Greater Than - Expected Output: True
(startEval '((define y 9) (equal? y 8))) ; Object Equality - Expected Output: False
(startEval '((define x 5) (equal? x 5))) ; Object Equality - Expected Output: True
(startEval '(if (= (* 7 3) (+ 15 7)) (+ 5 4) (* 7 8))) ; if Condition - Expected Output: 56
(startEval '(if (= (* 7 3) (+ 15 6)) (+ 5 4) (* 7 8))) ; if Condition - Expected Output: 9
(startEval '((lambda (x y z) (+ x z)) 1 3 5)) ; lambda expression - Expected Output: 6
(startEval '(let ([x 5]) (+ x 5))) ; let binding - Expected Output: 10
(startEval '(let ([f (lambda (x) (* x x))]) (f 5))) ; let-lambda binding - Expected Output: 25
(startEval '(letrec ([f (lambda (x) (if (= x 1) 1
                                        (* x (f (- x 1)))))])
              (f 10))) ; letrec binding - Expected Output: 3628800
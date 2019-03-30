#lang racket

(require "operators.rkt")
(require "lists.rkt")

(define (addBinding newBinding currentBindings)
  (cond
    [(symbol? (car newBinding)) (append (list newBinding) currentBindings)]
    [else (append (list (cdr newBinding)) currentBindings)])
  )
  
(define (bindLambdaVariables variables values)
  (if (null? variables) '()
      (append (bindLambdaVariables (cdr variables) (cdr values)) (list (list (car variables) (car values)))))
  )

(define (findBinding variable bindings)
  (cond
    [(null? bindings) -1]
    [(equal? variable (car (car bindings))) (car (cdr (car bindings)))]
    [else (findBinding variable (cdr bindings))])
  )

(define (startEval program)
  (myEval program '())
  )

(define (myEval program bindings)
  (cond
    ; If the program is just an empty list, say fuck it and return 0
    [(equal? program '()) 0]
    ; Get a variable
    [(symbol? program) (myEval (findBinding program bindings) bindings)]
    ; If the program is not a list, just return whatever it is
    [(not (list? program)) program]
    ; If program is a list of only one list, evaulate the inner list
    [(and (list? program) (null? (cdr program))) (myEval (car program) bindings)]
    ; Process any defines
    [(and (list? (car program)) (equal? (car (car program)) 'define)) (myEval (cdr program) (addBinding (cdr (car program)) bindings))]
    ; Process lambdas
    [(and (list? (car program)) (equal? (car (car program)) 'lambda)) (myEval (cdr (cdr (car program))) (append (bindLambdaVariables (car (cdr (car program))) (cdr program)) bindings))]
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
    [(equal? (car program) 'lambda) (myEval (car (cdr (cdr program))))]
    ; Test for let
    [(equal? (car program) 'let) (myEval (cdr (cdr program)) (addBinding (car (car (cdr program))) bindings))]
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
(startEval '(letrec ([fact (lambda (x) (if (= x 0) (quote 1)
                                          (* x (fact (- x 1)))))])
              (fact 10)))

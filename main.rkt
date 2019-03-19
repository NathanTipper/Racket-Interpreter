#lang racket

(require "operators.rkt")
(require "lists.rkt")

(define (addBinding newBinding currentBindings)
  (append (list (cdr newBinding)) currentBindings)
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
    ; Process any defines
    [(and (list? (car program)) (equal? (car (car program)) 'define)) (myEval (cdr program) (addBinding (car program) bindings))]
    ; If the program is not a list, just return whatever it is
    [(not (list? program)) program]
    ; If the program is a list and all there is just the list, return the list inside the program NOT SURE IF THIS IS NEEDED
    [(and (list? program) (null? (cdr program))) (car program)]
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
    [(equal? (car program) '>) (isGreaterThan? (myEval (car (cdr program) bindings)) (myEval (car (cdr (cdr program))) bindings))]
    ; Test less than
    [(equal? (car program) '<) (isLessThan? (myEval (car (cdr program) bindings)) (myEval (car (cdr (cdr program))) bindings))]
    ; Test less than or equal to
    [(equal? (car program) '<=) (isLessOrEqualThan? (myEval (car (cdr program) bindings)) (myEval (car (cdr (cdr program))) bindings))]
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
    ; Get a variable
    [(symbol? (car program)) (findBinding (car program) bindings)]
    [else program])
  )

(startEval '(if(= (+ 2 2) (+ 4 1)) 2 3))
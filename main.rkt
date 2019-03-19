#lang racket

(require "basic.rkt")
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
    [(equal? program '()) 0]
    [(not (list? program)) program]
    [(and (list? program) (null? (cdr program))) (car program)]
    [(and (list? (car program)) (equal? (car (car program)) 'define)) (myEval (cdr program) (addBinding (car program) bindings))]
    [(and (list? (car program)) (null? (cdr program))) (myEval (car program) bindings)]
    [(equal? (car program) 'quote) (myEval (cdr program) bindings)]
    [(equal? (car program) '+) (add (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '-) (sub (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '*) (mult (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '/) (div (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '=) (isEqual? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '>) (isGreaterThan? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '<) (isLessThan? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '<=) (isLessOrEqualThan? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '>=) (isGreaterOrEqualThan? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) 'equal?) (areEqual? (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) 'car) (myEval (getCar (cdr program)) bindings)]
    [(equal? (car program) 'cdr) (myEval (getCdr (cdr program)) bindings)]
    [(equal? (car program) 'cons)(cons (myEval (car (cdr program)) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) 'pair?) (isPair? (myEval (cdr program) bindings))]
    [(symbol? (car program)) (findBinding (car program) bindings)]
    [else program])
  )

(startEval '(pair? (1 2 3 4)))
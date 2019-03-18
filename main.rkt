#lang racket

(require "basic.rkt")

(define (startEval program)
  (myEval program '())
  )

(define (myEval program bindings)
  (cond
    [(equal? (car program) '+) (add (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '-) (sub (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '*) (mult (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [(equal? (car program) '/) (div (myEval (cdr program) bindings) (myEval (cdr (cdr program)) bindings))]
    [else (car program)])
  )

(startEval '(* 5 4))
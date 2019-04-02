#lang racket

; given the program, get the first thing within the program
(provide getCar)
(define (getCar program)
  (car (car program))
  )

; given the program, the rest of the program
(provide getCdr)
(define (getCdr program)
  (cdr (car program))
  )

; is x a pair?
(provide isPair?)
(define (isPair? x)
  (pair? x)
  )
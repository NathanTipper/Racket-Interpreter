#lang racket

(provide getCar)
(define (getCar program)
  (car (car program))
  )

(provide getCdr)
(define (getCdr program)
  (cdr (car program))
  )

(provide isPair?)
(define (isPair? x)
  (pair? x)
  )
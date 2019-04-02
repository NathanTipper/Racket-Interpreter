#lang racket

; Add x and y 
(provide add)
(define (add x y)
  (+ x y))

; Subtract y from x
(provide sub)
(define (sub x y)
  (- x y))

; Multiply x by y
(provide mult)
(define (mult x y)
  (* x y))

; Divide x by y
(provide div)
(define (div x y)
  (/ x y))

; Are x and y equal in value?
(provide isEqual?)
(define (isEqual? x y)
  (= x y))

; Is x less than y?
(provide isLessThan?)
(define (isLessThan? x y)
  (< x y))

; Is x greater than y?
(provide isGreaterThan?)
(define (isGreaterThan? x y)
  (> x y))

; Is x less than or equal to y
(provide isLessThanOrEqual?)
(define (isLessThanOrEqual? x y)
  (<= x y))

; Is x greater than or equal to y
(provide isGreaterThanOrEqual?)
(define (isGreaterThanOrEqual? x y)
  (>= x y))

; Are x and y equal?
(provide areEqual?)
(define (areEqual? x y)
  (equal? x y))
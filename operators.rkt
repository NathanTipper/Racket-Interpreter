#lang racket

(provide add)
(define (add x y)
  (+ x y))

(provide sub)
(define (sub x y)
  (- x y))

(provide mult)
(define (mult x y)
  (* x y))

(provide div)
(define (div x y)
  (/ x y))

(provide isEqual?)
(define (isEqual? x y)
  (= x y))

(provide isLessThan?)
(define (isLessThan? x y)
  (< x y))

(provide isGreaterThan?)
(define (isGreaterThan? x y)
  (> x y))

(provide isLessOrEqualThan?)
(define (isLessOrEqualThan? x y)
  (<= x y))

(provide isGreaterOrEqualThan?)
(define (isGreaterOrEqualThan? x y)
  (>= x y))

(provide areEqual?)
(define (areEqual? x y)
  (equal? x y))
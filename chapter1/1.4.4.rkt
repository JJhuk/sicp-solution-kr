#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/ (f (- x dx))
       (f x)
       (f (+ x dx))
       3)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))  

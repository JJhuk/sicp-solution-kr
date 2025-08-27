#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc n)
  (+ 1 n))

((double inc) 3)

(((double (double double)) inc) 5)
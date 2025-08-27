#lang sicp

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (pow n m)
  (if (= m 1)
      n
      (* n (pow n (- m 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root x n damp-count)
  (fixed-point
   ((repeated average-damp damp-count)
    (lambda (y)
      (/ x (pow y (- damp-count 1)))))
   1.0))

(nth-root 2 5 2)

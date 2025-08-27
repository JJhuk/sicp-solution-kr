#lang sicp

(define tolerance 0.0001)
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))


;1.1.7 sqrt

(define (iterative-improve-1 good-enough? improve)
  (lambda (x)
    (define (try guess)
      (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))
    (try 1.0)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (sqrt-2 x)
  ((iterative-improve-1
    good-enough?
    improve) 1.0))

(sqrt-2 9)

;1.3.3 fixed-point

(define (iterative-improve-2 good-enough? improve)
  (lambda (x)
    (define (try guess)
      (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (try next))))
    (try 1.0)))


(define (fixed-point-2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve-2 close-enough? f) first-guess))

(fixed-point-2 cos 1.0)
;Q 1.46
                 
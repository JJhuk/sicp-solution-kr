#lang sicp

(define (make-rat n d)
  (define (positive? x) (> x 0))
  (define (flip-sign x) (* x -1))
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (let ((g (gcd n d)))
    (if (positive? (* n d))
        (cons (- (abs (/ n g))) (abs (/ d g))))
        (cons (abs (/ n g)) (abs (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

;1/2
(print-rat one-half)

(define one-third (make-rat 1 3))

;5/6
(print-rat (add-rat one-half one-third))

;1/6
(print-rat (mul-rat one-half one-third))

;6/9 if use gcd, 2/3
(print-rat (add-rat one-third one-third))

;2.1
(print-rat (make-rat -3 -6))

(print-rat (make-rat -3 6))

(print-rat (make-rat 3 -6))

(print-rat (make-rat 3 6))



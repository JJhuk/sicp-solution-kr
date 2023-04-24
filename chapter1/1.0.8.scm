#lang sicp

(define (square x)
    (* x x))

(define (improve guess x)
    (/  (+  (/ x (square guess))
            (* 2 guess))
        3))

(define (good-enough? guess x)
    (= guess (improve guess x)))

(define (cube-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-iter (improve guess x)
                    x)))

(define (cube-root x)
    (cube-iter 1.0 x))

(cube-root 27)
; ;Value: 3.0
(cube-root 8)
; ;Value: 2.0
(cube-root 99990)

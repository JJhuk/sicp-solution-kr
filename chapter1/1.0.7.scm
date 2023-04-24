#lang sicp

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (square x)
    (* x x))


; old version

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                        x)))
(define (sqrt x)
    (sqrt-iter 1.0 x))

; my version
    
(define (my-good-enough? guess x)
    (= guess (improve guess x)))

(define (my-sqrt-iter guess x)
    (if (my-good-enough? guess x)
        guess
        (my-sqrt-iter (improve guess x)
                    x)))

(define (my-sqrt x)
    (my-sqrt-iter 1.0 x))


(square (sqrt 0.0009))
;0.0016241401856992538

(square (my-sqrt 0.0009))
;0.0009

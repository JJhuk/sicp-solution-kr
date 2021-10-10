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
(define (new-sqrt x)
    (sqrt-iter 1.0 x))

; my version
    
(define (my-good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.001))

(define (my-sqrt-iter guess x)
    (if (my-good-enough? guess x)
        guess
        (my-sqrt-iter (improve guess x)
                    x)))

(define (my-sqrt x)
    (my-sqrt-iter 1.0 x))


; 1 ]=> (square (sqrt 0.0009))

; ;Value: .0009

; 1 ]=> (square (new-sqrt 0.0009))

; ;Value: 1.6241401856992538e-3

; 1 ]=> (square (my-sqrt 0.0009))

; ;Value: 9.016608107957646e-4
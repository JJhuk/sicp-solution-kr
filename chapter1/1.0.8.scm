(define (square x)
    (* x x))

(define (improve guess x)
    (/  (+  (/ x (square guess))
            (* 2 guess))
        3))

(define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) 0.001))

(define (cube-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-iter (improve guess x)
                    x)))

(define (cube-root x)
    (cube-iter 1.0 x))

; 1 ]=> (cube-root 27)

; ;Value: 3.0000005410641766

; 1 ]=> (cube-root 8)

; ;Value: 2.000004911675504

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (square x)
    (* x x))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
            (else else-clause)))

(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                        x)))

; (define (sqrt-iter guess x)
;     (if (good-enough? guess x)
;         guess
;         (sqrt-iter (improve guess x)
;                     x)))

(define (new-sqrt x)
    (sqrt-iter 1.0 x))

;Aborting!: maximum recursion depth exceeded

;if 문은 predicate를 판별하고 다음 할 연산을 계산한다.
;new-if는 함수이기 떄문에 각 인자들이 치환되어야 하기 때문에 판별이 되야 하고
;sqrt-iter은 그 관점에서 재귀이기 때문에 무한루프에 빠지게 된다.
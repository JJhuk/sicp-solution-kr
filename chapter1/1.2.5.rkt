#lang sicp

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (new-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 1000037 100)

;기존 expmod 는 (x * y) mod m = ((x mod m) * (y mod m)) mod m 을 이용하는데,
;루프마다 지수 계산을 하더라도 mod 연산때문에 값의 크기가 확 작아진다.
;반면 new-expmod는 b^n = b^(n/2) * b^(n/2) (b > 0) 지수법칙을 사용하는데
; 저 지수의 값을 구하고 마지막에 mod연산을 실시한다.
;그말은 즉슨 값이 커지면 뒷 연산이 메모리가 감당을 못한다.

#lang sicp

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (fast-prime? n times)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base(- exp 1) m))
                      m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100)
         (report-prime (- (runtime) start-time))
         true)
        (else false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a)
  (define (loop a now find)
    (cond ((divides? 2 now) (loop a (+ 1 now) find))
          ((< find 3)
           (cond ((timed-prime-test now) (loop a (+ 2 now) (+ find 1)))
                 (else (loop a (+ 2 now) find))))))
  (loop a a 0))

(search-for-primes 10000000)
; 5번 검사했을 때 기준
; 1,003 나와서 컷


; 10번 검사했을 때 기준
; 1,009 1,013 1,019 찾는데 8 7 7
; 1,000,003 1,000,033 1,000,037 16 12 11 (기존은 25 24 24) 수가 커지면 커질수록 빨라진다.

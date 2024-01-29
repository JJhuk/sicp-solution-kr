#lang sicp

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (fast-prime? n times)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (* (expmod base (/ exp 2) m)
                         (expmod base (/ exp 2) m))
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

; square안쪽으로 가게 되면 n이 계산된 상태에서 곱해진다.
; 하지만 그냥 operation을 쓰면 좌항을 구한다음 우항도 또 똑같이 구하게 된다.
; 원래 even? 이면 반씩 줄어들게 되었는데 * 를 쓰면서 2배씩 늘어나게 되었다.

#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
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

(search-for-primes 1000000)


;root 10은 약 3.xxx
;10회 실시해서 평균값으로 계산
;1,000 처음 나오는 3개의 소수는
;1,009 1,013 1,019
;1 1 0
;1 1 0
;1 1 0
;1 1 2
;1 1 1
;1 1 2
;1 1 1
;2 1 0
;1 1 1
;1 1 1
;=> 1.1 1.0 0.8

;10,000 구간 처음 나오는 3개의 소수는
;10,007 10,009 10,037
;3 3 3
;2 3 3
;3 2 3
;2 3 3
;5 3 3
;3 2 2
;3 3 3
;3 3 3
;3 2 3
;3 3 3
;=> 3 2.7 2.9

;100,000 구간 처음 나오는 3개의 소수
;100,003 100,019 100,043
;8 8 9
;8 9 8
;10 9 9
;8 9 7
;9 8 8
;8 8 9
;8 9 10
;7 8 7
;8 8 9
;9 8 9
;=>8.3 8.4 8.5

;1,000,000 구간 처음 나오는 3개의 소수
;1,00,003 1,000,033 1,000,037
;26 26 25
;25 25 24
;35 25 29
;23 22 21
;28 25 24
;21 23 22
;25 23 24
;23 24 22
;25 24 26
;24 24 25
;=>25.5 24.1 24.2

;약 3배씩 늘어나는것을 볼 수 있다.

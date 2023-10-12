#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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
;1 0 1
;1 1 0
;1 2 1
;2 1 1
;0 1 1
;1 1 1
;1 0 1
;1 1 1
;0 1 1
;1 1 1
;=> 0.9 0.9 0.9 (최적화 전은 1.1 1.0 0.8)
;1.22배 1.11배 0.88배

;10,000 구간 처음 나오는 3개의 소수는
;10,007 10,009 10,037
;2 2 2
;2 3 2
;2 3 3
;2 3 2
;2 2 2
;2 2 2
;3 2 3
;2 2 2
;2 2 2
;2 2 3
;=> 2.1 2.3 2.3 (최적화 전은 3.0 2.7 2.9)
;1.42배 1.17배.1.26배

;100,000 구간 처음 나오는 3개의 소수
;100,003 100,019 100,043
;6 6 6
;9 6 6
;8 7 6
;9 7 7
;6 6 6
;6 5 7
;6 6 6
;8 8 6
;7 6 6
;6 6 6
;=>7.1 6.3 6.2 (최적화 전은 8.3 8.4 8.5)
;1.16배 1.33배 1.37배

;1,000,000 구간 처음 나오는 3개의 소수
;1,00,003 1,000,033 1,000,037
;21 21 19
;25 20 20
;22 20 21
;20 17 18
;18 20 19
;19 19 19
;20 19 19
;19 18 18
;19 19 18
;20 20 18
;=> 20.3 19.3 18.9 (최적화 전은 25.5 24.1 24.2)
;1.25배 1.24배 1.28배

;최적화 이후 약 1.22배 빨라짐.
;이론상 2배 빨라야 하는데, next 함수의 if문 연산때문에 느려진 것 같다.

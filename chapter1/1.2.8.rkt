#lang sicp

; expmod에서 제곱을 할 때마다 not (1 or n-1) &&
; 제곱한 값이 n으로 나눈 나머지가 1인지 체크
; 힌트 : 찾으면 뻔하지 않은 제곱근이 0
(define (remainder-square-checked x n)
  (if (and (not (or (= x 1)
                    (= x  (- n 1))))
           (= (remainder (* x x) n) 1))
      0
      (remainder (* x x) n)))

;
(define (expmod-checked base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder-square-checked (expmod-checked base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-checked base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (iter a)
    (= (expmod-checked a (- n 1) n) 1))
  (iter (+ 1 (random (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond  ((= times 0) #t)
         ((miller-rabin-test n)
          (miller-rabin-prime? n (- times 1)))
         (else #f)))

(miller-rabin-prime?    2 10)


;(1 mod n)의 뻔하지 않는 제곱근..
;P이 소수이고 (0 < a < P) 인 정수 a 
; a^2 mod P = 1 이라고 했을 때
; a^2  = NP + 1 (n >= 0 정수)
; a^2 - 1 = NP
; (a+1)(a-1) = NP
; P는 소수이므로 (a+1) 아니면 (a-1) 중 하나
; a < P  이므로 (a+1)  = P
; (a-1)  = N
; p가 소수려면 a는 P-1 이거나 a가 1이어야 함.
; a = 1도 아니고 p-1 도 아닌데 식을 만족하면 소수가 아님.
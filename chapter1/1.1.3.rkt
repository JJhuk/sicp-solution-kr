#lang sicp

(define (sqrt x)
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (= guess (improve guess x)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                    x)))
  (sqrt-iter 1.0 x))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define golden-rato-a (/ (+ 1 (sqrt 5))
                       2))

(define golden-rato-b (/ (- 1 (sqrt 5))
                       2))

;b는 음수가 아닌 정수
(define (pow a b)
  (define (pow-iter a b count accum)
    (cond ((= b 0) 1)
          ((= count b) accum)
          (else (pow-iter a b (+ count 1) (* accum a)))))
  (pow-iter a b 0 1))

(define (test n)
  (= (fib n)
     (/ (- (pow golden-rato-a n) (pow golden-rato-b n))
        (sqrt 5))))

;https://sicp-solutions.net/post/sicp-solution-exercise-1-13/
;F(n) = F(n-1) + F(n-2) 인 식에서
;F(n)/F(n-1) = 1 + (1 / F(n-2)/F(n-1)).
;n은 부등식 범위로 0을 수렴 안하므로
;lim 해주면 x = 1 + 1/x 로 수렴
(test 1)
(test 2)
(test 3)
(test 4)
(test 5)
(test 6)
(test 7)
(test 8)
(test 9)

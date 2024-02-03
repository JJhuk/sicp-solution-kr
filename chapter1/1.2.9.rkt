#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (cube x)
  (* x x x))

;문제의 n이 100과 1000만이므로 h는 짝수식을 사용.
(define (simpson a b f n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (simpson-term k)
    (cond ((= k 0) (y k))
          ((even? k) (* 4.0 (y k)))
          (else (* 2.0 (y k)))))
  (* (/ h 3)
     (sum simpson-term 0 inc n)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;참고로 0과 1 사이에서 cube 함수를 정적분 한 값은 1/4이다.
(simpson 0 1 cube 100) ;0.2567166666666668
(simpson 0 1 cube 1000) ;0.25066716666666605
(integral cube 0 1 0.01) ;0.24998750000000042
(integral cube 0 1 0.001) ;0.249999875000001

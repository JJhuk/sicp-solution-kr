#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; 선형 반복으로 수정
(define (iter-sum term a next b)
  (define (iter a result )
    (if (> a b) ;<??>
        result  ;<??>
        (iter (next a) (+ result (term a))))) ;<??> next <??> (+ result (term a))
  (iter a 0)) ; <??>a <??>0

(define (identity x) (* x x x))

(define (next x) (+ x 1))

(sum identity 0 next 10)
(iter-sum identity 0 next 10)

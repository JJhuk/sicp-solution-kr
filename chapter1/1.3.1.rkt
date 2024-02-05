#lang sicp

(define (next x) (+ x 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;1.3.0 풀이에서 변형
(define (iter-product term a next b)
  (define (iter a result )
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;짝수일 때 분자가 a면 분모는 a + 1
;홀수일 때 분자가 a+1면 분모 a
(define (term a)
        (if (even? a)
            (/ a (+ a 1.0))
            (/ (+ a 1.0) a)))

(* (product term 2 next 100) 4) ; 3.141591082795153
(* (iter-product term 2 next 100) 4) ;3.1415910827949127


#lang sicp

(define (mul a b)
  (define (double a)
    (* 2 a))
  (define (halve a)
    (/ a 2))
  (define (even? n)
    (= (remainder n 2) 0))
  (define (mul-core a b result)
    (cond ((even? b) (mul-core a (halve b) (double result)))
          ((= b 1) result)
          (else (mul-core a (- b 1) (+ a result)))))
  (mul-core a b a)
)

(mul 11 2)
(mul 11 11)
(mul 11 12)
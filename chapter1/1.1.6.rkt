#lang sicp

(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square n)
    (* n n))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 4 4)

(define (expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (expt-iter b n a)
    (cond ((= n 1) (* a b))
          ((= a 1) (expt-iter b (/ n 2) (* b b)))
          (else (expt-iter b (/ n 2) (* a a)))))
  (cond ((even? n) (expt-iter b n 1))
        (else (b * (expt-iter b (- n 1) 1)))))

(expt 4 1)
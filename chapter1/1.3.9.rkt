#lang sicp

(define (cont-frac n d k)
  (define (rec i)
    (cond ((> i k) 1)
          (else (/ (n i) (+ (d i) (rec (+ i 1)))))))
  (rec 1.0))

(define (lambett x k)
  (define (lambett-n i)
    (cond ((= i 1) x)
    (else (* x x -1))))
  (define (lambett-d n)
    (- (* 2 n) 1))
  (cont-frac lambett-n lambett-d k))

;tan(5 rad) = -3.38051500625

(lambett 5 10000)
;-3.3805150062465867

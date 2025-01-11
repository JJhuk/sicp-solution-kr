#lang sicp
(define (de-frac-continuis n)
  (cond ((<= n 2) n)
        ((= (remainder (+ n 1) 3) 0) (* (+ (quotient n 3) 1) 2))
        (else 1)))

(define (cont-frac n d k)
  (define (rec i)
    (cond ((> i k) 1)
          (else (/ (n i) (+ (d i) (rec (+ i 1)))))))
  (rec 1.0))

;; exp = 2.718281828459045
;; exp-2 = f
;; f = 0.718281828459045

(cont-frac (lambda (i) 1)
           de-frac-continuis
           100)

;;0.7182818284590453
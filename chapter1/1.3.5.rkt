#lang sicp

(define tolerance 0.00001)

(define (golden-ratio x f)
  (define (close-enough? x y) (< (abs (- x y)) tolerance))
  (let ((test-value (f x)))
    (cond ((close-enough? test-value x) test-value)
          (else (golden-ratio test-value f)))))


(golden-ratio 1.0 (lambda (x) (+ 1.0 (/ 1.0 x))))
;1.6180327868852458
;1.618033988749....

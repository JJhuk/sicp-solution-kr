#lang sicp

(define (term a) a)
(define (next a) (+ a 1))

(define (accumulate-re combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-re combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b result)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
      

(accumulate-re + 0 term 1 next 10)
(accumulate-re * 1 term 1 next 10)
(accumulate-iter + 0 term 1 next 10 1)
(accumulate-iter * 1 term 1 next 10 1)

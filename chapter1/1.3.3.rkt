#lang sicp

(define (next a) (+ a 1))

(define (filtered-accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (combiner (if (predicate a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner null-value term (next a) next b predicate))))

(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
  (define (square n) (* n n))
  (define (divides? a b) (= (remainder b a) 0))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (q-a a b)
  (define (q-a-term x) (* x x))
  (filtered-accumulate + 0 q-a-term a next b prime?))

(define (q-b n)
  (define (disjoint? i)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (and (< i n)
       (< 0 i)
       (= (gcd i n) 1)))
  (define (identity x) x)
  (filtered-accumulate * 1 identity 1 next n disjoint?))


(q-a 1 10)
(q-b 20)

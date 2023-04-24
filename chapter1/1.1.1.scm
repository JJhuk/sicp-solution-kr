#lang sicp

(define (re-function n)
  (cond ((< n 3) n)
        (else ( + (re-function (- n 1))
                  (* 2 (re-function (- n 2)))
                  (* 3 (re-function (- n 3)))))))


(define (iter-function n)
  (cond ((< n 3 ) n)
        (else (iter n 1 2 1 0))))

  
(define (iter n count a b c)
  (cond ((= (- n 2) count) (+ a (* 2 b) (* 3 c)))
        (else (iter n
                    (+ count 1)
                    (+ a (* 2 b) (* 3 c))
                    a
                    b))))


(= (re-function 6) (iter-function 6))
(= (re-function 5) (iter-function 5))
(= (re-function 4) (iter-function 4))
(= (re-function 3) (iter-function 3))
(= (re-function 2) (iter-function 2))
(= (re-function 1) (iter-function 1))

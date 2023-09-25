#lang sicp

;0 T_pq : (a, b)
;1 T_pq : (bq + aq + ap, bp + aq)
;여기서 bq + aq + ap 를 a_2 , bp + aq 를 b_2로 둔다.
;2 T_pq : (b_2q + a_q + a_p, b_2p + a_2q)
;a_2와 b_2 에 대입하면
;a_3 = a(p^2 + 2pq + q^2 + q^2) + b(p^2 + q^2)
;b_3 = a(q^2 + 2pq) + b(p^2 + q^2)
;p' = p^2 + q^2, q' = q^2 +2pq 로 두면
;a_3 = a(p'+q') + bq', b_3 = aq' + bp' 이다.


(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (square n)
    (* n n))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ;p'
                   (+ (square q) (* 2 p q)) ;q'
                   (/ count 2)))
        
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b q) (* a q))
                        p
                        q
                        (- count 1)))))
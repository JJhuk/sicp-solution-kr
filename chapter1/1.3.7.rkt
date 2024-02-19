#lang sicp

(define (cont-frac n d k)
  (define (rec i)
    (cond ((> i k) 1)
          (else (/ (n i) (+ (d i) (rec (+ i 1)))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)

;황금비는 x^2 = x + 1
; x = (x+1)x
; x = 1 + 1/x
; x -1 = 1/x 이므로 황금비의 역수는 황금비에 -1 을 한 것
;1.6180.. -1 = 0.6180
;k가 5일때
;0.6153846153846154
;k가 10일때
;0.6180555555555556

;반복하는 방식은 역으로 올라가야 한다.
(define (cont-frac-iter n d k)
  (define (iter i result)
    (cond ((> i 1) result)
          (else (iter (- i 1) (/ (n i) (+ (d i) result))))))
  (iter k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)

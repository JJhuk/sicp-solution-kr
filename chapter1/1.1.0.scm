;에커만 함수를 나타낸 프로시저이다.
(define (A x y)
    (cond ((= y 0) 0)
            ((= x 0) (* 2 y))
            ((= y 1) 2)
            (else (A (- x 1)
                    (A x (- y 1))))))


;Q1 아래의 식의 값은 무엇인가

;(A 1 10)
; => 
; A (0 A(1 9))
; A (0 A(0 A(1 8)))
; ....요약
; A (0 ... A(0 A(1 1))) y가 1이므로 2 리턴
; A (0 ... A(0 2)) x가 0이므로 y값에 2 곱함
; 여기서 A(0) 이 n개만큼 있기 때문에 y값(2) 에 2를 곱하는 횟수가 n번
; 따라서 2^n 이라고 줄일 수 있고 2^10 = 1024이다.

;(A 2 4)
; =>
; A(1 (A (2 3)))
; A(1 (A 1 A(2 2)))
; A(1 (A 1 A(1 A(2 1)))))
; 여기서 A(2 1) 부분이 y가 1이므로 A(1 A(2 1))부분이 A(1 2)가 된다.
; 그런데 위 예시에서 (A 1 n) => 2^n이라고 정리했으므로,A(1 A(2,1)) 은 2^A(2, 1)이라고 볼 수 있다..
; 같은 맥락으로 A(1 2^A(2,1))은 => 2^2^A(2,1)
; (A 2 n)일때 n이 0과 1이 아닌 경우엔 2^2^2^2... n번 곱해줄 수 있다고 정리할 수 있다.
; 즉, 주어진 (A 2 4)에서 n에 해당하는 4가 1과 0이 아니므로 2^2^2^2 = 2^16 = 65536이다.

;(A 3 3)
; =>
; A(2 (A 3 2))
; A(2 (A 2 (A 3 1)))
; 여기서 (A 3 1) 부분에서 t가 1이므로 2가 된다.
; A(2 (A 2 2)) 인데 A(2 2)는 2^2 이므로 4고
; A(2 4) 로 변환할 수 있다.
; A(2 4)는 2^2^2^2 이므로 65536이다.


;다음 프로시저가 정의되어 있다고 하자.

((define (f n)
        (A 0 n)))

(define (g n)
        (A 1 n))

((define (h n)
        (A 2 n)))

(define (k n)
        (* 5 n n))

;0보다 큰 정수 n이 있을때 f, g, h의 프로시저의 기능을 수학으로 정의해 보라. 예를 들면 위 프로시저 k는 5n^2이다.

;f는 x가 0에 해당하므로 (* 2 n)인 2n으로 나타낼 수 있다.

;g는 이전 문제에서 2^n으로 나타낼 수 있다.

;h 는 2^2^2...거듭제곱을 거듭제곱한 횟수가 n번이다. 예를들어 n이 5면 2^2^2^2^2
        


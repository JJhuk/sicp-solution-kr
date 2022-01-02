; n < 3 일ㅐ f(n) = n
; n >= 3 일땐 f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 2)


; 위 식을 만족하는 함수를 recursive process 와 iterate process 로 구현하여라


; recursive process
(define (recursive-process n)
    (cond 
        ((< n 3) n)
        ((>= n 3) (+ 
                (recursive-process (- n 1))
                (* 2 (recursive-process (- n 2)))
                (* 3 (recursive-process (- n 3)))
            )
        )
    )
)

; iterate process
; n >= 3이될때 f(3) = f(2) + 2f(1) + 3f(0)
; 그래서 기준이 되는 2 1 0 을 a b c로 둔다.
; iterate를 진행하면서 각 단계의 a b c 를 치환하는 방식으로 접근
; 하지만, n < 3 일때 f(n) = n 이므로 반복한 횟수의 값을 활용하여 치환하는 방식을 구분.
; 예를들면 n이 4이고 첫번째 루프일때 f(4) 단계읜 f(3)은 f(count + 2) 이며 count + 2 >= 3 이기 때문에
; 다음 a는 현재의 (+ a (* 2 b) (* 3 c))


(define (iterate a b c count end)
    (if (= count end)
        (+ a (* 2 b) (* 3 c))
        (iterate
            (if (>= (+ count 2) 3)
                (+ a (* 2 b) (* 3 c))
                (+ count 2)
            )
            (if (>= (+ count 1) 3)
                a
                (+ count 1)
            )
            (if (>= (- count 0) 3)
                b
                count
            )
            (+ count 1)
            end
        )
    )
)

(define (iterate-process n)
    (cond
        ((< n 3) 
            n
        )
        ((>= n 3) 
            (iterate 2 1 0 1 (- n 2))
        )
    )
)


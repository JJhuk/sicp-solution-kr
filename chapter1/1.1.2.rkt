(define (combination m n)
  (cond ((or (= n 0) (= m n)) 1)
        (else (+ (pascal (- m 1) (- n 1))
                 (pascal (- m 1) n)))))

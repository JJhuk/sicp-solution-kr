(define (foo a b c)
    (if
        (and (< a b) (< a c))
        (+ (* b b) (* c c))
        (if
            (and (< b a) (< b c))
            (+ (* a a) (* c c))
            (+ (* a a) (* b b))
        )
    )
)
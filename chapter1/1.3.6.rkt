#lang sicp

(define tolerance 0.0001)

(define (fixed-point f first-guess count)
  (define (good-enough? x y) (< (abs (- x y)) tolerance))
  (let ((next (f first-guess)))
    (cond ((good-enough? first-guess next) next)
          ((= first-guess next) next)
          (else
           (display ";count: ")
           (display count)
           (display " next: ")
           (display next)
           (newline)
           (fixed-point f next (+ count 1))))))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0 1)
;count: 1 next: 9.965784284662087
;count: 2 next: 3.004472209841214
;count: 3 next: 6.279195757507157
;count: 4 next: 3.759850702401539
;count: 5 next: 5.215843784925895
;count: 6 next: 4.182207192401397
;count: 7 next: 4.8277650983445906
;count: 8 next: 4.387593384662677
;count: 9 next: 4.671250085763899
;count: 10 next: 4.481403616895052
;count: 11 next: 4.6053657460929
;count: 12 next: 4.5230849678718865
;count: 13 next: 4.577114682047341
;count: 14 next: 4.541382480151454
;count: 15 next: 4.564903245230833
;count: 16 next: 4.549372679303342
;count: 17 next: 4.559606491913287
;count: 18 next: 4.552853875788271
;count: 19 next: 4.557305529748263
;count: 20 next: 4.554369064436181
;count: 21 next: 4.556305311532999
;count: 22 next: 4.555028263573554
;count: 23 next: 4.555870396702851
;count: 24 next: 4.555315001192079
;count: 25 next: 4.5556812635433275
;count: 26 next: 4.555439715736846
;count: 27 next: 4.555599009998291
;count: 28 next: 4.555493957531389
;=> 4.555563237292884

(fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2.0)) 2.0 1)

;count: 1 next: 5.9828921423310435
;count: 2 next: 4.922168721308343
;count: 3 next: 4.628224318195455
;count: 4 next: 4.568346513136242
;count: 5 next: 4.5577305909237005
;count: 6 next: 4.555909809045131
;count: 7 next: 4.555599411610624
;=> 4.5555465521473675

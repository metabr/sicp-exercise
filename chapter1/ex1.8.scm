(define (square x) (* x x))

(define (cube x) (* x x x))

;(define (good-enough? guess x)
;  (< (abs (- (cube guess) x)) 0.001))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.0001))

;
; (x/y^2 + 2y) / 3
;
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cubert-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubert-iter (improve guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))

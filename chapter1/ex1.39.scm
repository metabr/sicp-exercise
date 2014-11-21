(define (square x) (* x x))

(define (inc i) (+ i 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
      x
      (square x)))
  (define (d i)
    (+ i
       (- i 1)))
  (define (frac d n i k)
    (if (> i k)
      0
      (/ (n i) (- (d i) (frac d n (inc i) k)))))
  (frac d n 1.0 k))

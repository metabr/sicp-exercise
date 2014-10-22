(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose (repeated f (- n 1)) f)))

(newline)
(display ((repeated square 2) 5))

(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(newline)
(display (((double (double double)) inc) 5))

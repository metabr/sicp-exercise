(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f   
    (compose (repeated f (- n 1)) f)))

(define (smooth f)
  (lambda (x) (/ (+
		   (f x)
		   (f (+ x dx))
		   (f (- x dx)))
		 3.0)))

(define dx 0.00001)

(define (inc x) (+ x 1))

(define (cont-frac d n k)
  (define (frac d n i k)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (frac d n (inc i) k)))))
  (frac d n 1 k))

(display (/ 1 
	    (cont-frac (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       20)))

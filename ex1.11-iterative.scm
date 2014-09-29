(define (f n)
  (if (< n 3)
    n
    (f-iter 2 1 0 (- n 2))))

(define (f-iter n1 n2 n3 r)
  (if (= r 0)
    n1
    (f-iter (+ n1 (* 2 n2) (* 3 n3))
	    n1
	    n2
	    (- r 1))))

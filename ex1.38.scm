(define (inc x) (+ x 1)) 

(define (cont-frac d n k)
  (define (frac d n i k)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (frac d n (inc i) k)))))
  (frac d n 1 k))

(define (euler-d i)
  (cond ((= i 1) 1)
	((= i 2) 2)
	((= i 3) 1)
	(else (if (= 1 (euler-d (- i 3)))
		1
		(+ (euler-d (- i 3)) 2)))))


(newline)
(display "Approximation of e based on Euler's expansion: ")
(display (+ 2
	    (cont-frac euler-d
		       (lambda (i) 1.0)
		       20)))

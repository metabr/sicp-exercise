(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (* a b)
  (if (= b 0) 0 (mult-iter a b 0)))

(define (mult-iter a b x)
  (cond ((= b 1) (+ a x))
	((even? b) (mult-iter (double a) (halve b) x))
	(else (mult-iter (double a) (halve (- b 1)) (+ x a)))))

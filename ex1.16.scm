(define (square x) (* x x))

(define (fast-exp b n)
  (exp-iter 1 b n))

(define (exp-iter a b n)
  (cond ((= n 0) a)
	((even? n) (exp-iter a (square b) (/ n 2)))
	(else (exp-iter (* a b) (square b) (/ (- n 1) 2)))))

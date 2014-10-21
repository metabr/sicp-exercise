(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (cube x) (* x x x)) 

(define (inc x) (+ x 1)) 

(define (identity x) x)

(newline)
(display "Sum of cubes of 1 through 10: ")
(display (sum cube 1 inc 10))
(newline)
(display "Factorial of 5: ")
(display (product identity 1 inc 5))
(newline)

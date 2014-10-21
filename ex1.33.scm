; required definition of general version of accumulate
(define (filtered-accumulate combiner null-value term a next b filter-predicate)
  (if (> a b)
    null-value
    (combiner (if (filter-predicate a) (term a) null-value)
	      (filtered-accumulate combiner null-value term (next a) next b filter-predicate))))

; prime predicate
(define (smallest-divisor n)
  (find-divisor n 2)) 

(define (next n)
  (if (= n 2)
    3   
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0)) 

(define (prime? n)
  (= n (smallest-divisor n)))

;sum of squares of the prime numbers in the interval of a to b
(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(newline)
(display "Sum of squares of prime numbers between 1 and 10: ")
(display (sum-of-prime-squares 1 10))
(newline)

; product of all positive integers less than n that are relatively prime to n
; (i.e., all positive integers i < n such that GCD(i, n) = 1)
(define (identity x) x)

(define (product-relative-prime n)
  (define (relative-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))

(newline)
(display "Product of all positive integers less than n = 10 that are relatively prime to n: ")
(display (product-relative-prime 10))
(newline)

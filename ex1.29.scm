(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b n)
  (define (h)
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (coeff k)
    (cond ((= k 0) 1)
	  ((= k n) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (term k)
    (* (coeff k) (y k)))
  (* (/ (h) 3)
     (sum term 0 inc n)))

(newline)
(display "Result for n = 100: ")
(display (integral cube 0.0 1.0 100))
(newline)

(newline)
(display "Result for n = 1000: ")
(display (integral cube 0.0 1.0 1000))
(newline)

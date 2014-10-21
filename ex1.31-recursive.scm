(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(newline)
(display (factorial 5))

(define (pi n)
  (define (even-term n)
    (if (= n 1)
      2
      (if (even? n)
	(+ 2 (even-term (- n 1)))
	(even-term (- n 1)))))
  (define (odd-term n)
    (if (= n 1)
      3
      (if (odd? n)
	(+ 2 (odd-term (- n 1)))
	(odd-term (- n 1)))))
  (* 4.0
     (/ (product even-term 1 inc n)
	(product odd-term 1 inc n))))

(newline)
(display (pi 100))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	   (make-product (exponent exp)
			 (make-exponentiation (base exp)
					      (make-sum (exponent exp) '-1)))
	   (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (operation? x)
  (or (sum? x)
      (product? x)
      (exponentiation? x)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (make-sum (caddr s) (cdddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (make-product (caddr p) (cdddr p)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-sum a1 a2)
  (cond ((eq? a2 '()) a1)
	((and (not (operation? a2)) (pair? a2)) (list '+ a1 (make-sum (car a2) (cdr a2))))
	((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((eq? m2 '()) m1)
	((and (not (operation? m2)) (pair? m2)) (list '* m1 (make-product (car m2) (cdr m2))))
	((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((and (number? b) (number? e)) (expt b e))
	(else (list '** b e))))

(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (deriv '(* x y (+ x 3)) 'x))

(newline)
(display (deriv '(+ (* 2 (** x 2)) (* 4 x) 18) 'x))

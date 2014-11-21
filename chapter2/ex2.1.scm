(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; reducing to lowest terms in constructor
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; exercise 2.1 solution
(define (make-rat n d)
    (let ((g (abs (gcd d n))))
      (if (or (and (negative? n)
		   (negative? d))
	      (and (positive? n)
		   (negative? d)))
	(cons (- (/ n g)) (- (/ d g)))
	(cons (/ n g) (/ d g)))))

(print-rat (make-rat -3 9))
(print-rat (make-rat 4 -12))
(print-rat (make-rat -2 -6))

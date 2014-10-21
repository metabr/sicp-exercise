(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "Guess: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

(newline)
(display "x^x = 1000")
(newline)

(display "Searching for solution: ")
(newline)
(display (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
(newline)

(display "Searching for solution (with average damping): ")
(newline)
(display (fixed-point (lambda (y) (average (/ (log 1000) (log y)) y)) 2.0))
(newline)


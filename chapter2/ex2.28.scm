(define (fringe x)
  (cond ((null? x) x)
	((pair? (car x)) (append (fringe (car x))
				 (fringe (cdr x))))
	(else (append (list (car x))
		      (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))

(newline)
(display (fringe x))
(newline)
(display (fringe (list x x)))

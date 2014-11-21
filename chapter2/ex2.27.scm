(define (reverse x)
  (if (null? x)
    x
    (append (reverse (cdr x))
	    (list (car x)))))

(define (deep-reverse x)
  (cond ((null? x) x)
	((pair? (car x)) (append (deep-reverse (cdr x))
			         (list (deep-reverse (car x)))))
	(else (append (deep-reverse (cdr x))
		      (list (car x))))))

(define x (list (list 1 2) (list 3 4)))

(newline)
(display (deep-reverse (list 1 2 3 4)))
(newline)
(display x)
(newline)
(display (reverse x))
(newline)
(display (deep-reverse x))

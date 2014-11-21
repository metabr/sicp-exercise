(define (same-parity x . w)
  (let ((check? (if (odd? x)
		  odd?
		  even?)))
    (define (process-list in out)
      (if (null? in)
	out
	(if (check? (car in))
	  (process-list (cdr in) (append out (list (car in))))
	  (process-list (cdr in) out))))
    (process-list w (list x))))

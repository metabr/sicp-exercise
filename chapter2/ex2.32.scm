(define set (list 1 2 3))

; I did not solve this by myself.

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (append (list (car s)) x))
			rest)))))

(newline)
(display (subsets set))

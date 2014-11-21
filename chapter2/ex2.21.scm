(define (square-list items)
  (if (null? items)
    '()
    (cons (* (car items) (car items))
	  (square-list (cdr items)))))

(newline)
(display (square-list (list 1 2 3 4)))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

(newline)
(display (square-list (list 1 2 3 4)))

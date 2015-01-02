(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
	(else (union-set
		(adjoin-set (car set2) set1)
		(cdr set2)))))

(newline)
(display (element-of-set? 2 '(2 3 2 1 3 2 2)))
(newline)
(display (intersection-set '(2 3 2 1 3 2 2) '(1 2)))
(newline)
(display (union-set '(2 3 2 1 3 2 2) '(1 2)))

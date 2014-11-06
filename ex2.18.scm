(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l)) (list (car l)))))

(define test-list (list 1 4 9 16 25))

(newline)
(display test-list)
(newline)
(display (reverse test-list))

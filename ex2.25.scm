(define l1 (list 1 3 (list 5 7) 9))

(newline)
(display l1)
(newline)
(display (car (cdr (car (cdr (cdr l1))))))

(define l2 (list (list 7)))

(newline)
(display l2)
(newline)
(display (car (car l2)))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(newline)
(display l3)
(newline)
(display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))))

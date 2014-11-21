;; This implementation does not work as intended because of evaluation order.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(newline)
(display (square-list (list 1 2 3 4)))

;; This fixed implementation makes nested pairs instead of lists due to
;; switched order of arguments of cons.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(newline)
(display (square-list (list 1 2 3 4)))

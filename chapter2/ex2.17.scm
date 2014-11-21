(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(newline)
(display (list 23 72 149 34))
(newline)
(display (last-pair (list 23 72 149 34)))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(newline)
(display (reverse (list 1 2 3 4 5)))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(newline)
(display (reverse (list 1 2 3 4 5)))

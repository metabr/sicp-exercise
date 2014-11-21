(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (cons-iter n z result)
  (if (= (remainder z n) 0)
    (cons-iter n (/ z n) (+ result 1))
    result))

(define (car z)
  (cons-iter 2 z 0))

(define (cdr z)
  (cons-iter 3 z 0))

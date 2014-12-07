(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)                         ; empty set?
      (list '())                        ; sequence containing empty set
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-triplets n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
		 (map (lambda (k) (list i j k))
		      (enumerate-interval 1 (- j 1))))
	       (enumerate-interval 2 (- i 1))))
    (enumerate-interval 3 n)))

(define (make-triplet-sum t)
  (accumulate + 0 t))

; Find all ordered triples of distinct positive integers i, j, and k less than
; or equal to a given integer n that sum to a given integer s.
(define (ex41 n s)
  (flatmap (lambda (t) (permutations t))
       (filter (lambda (t) (= (make-triplet-sum t) s))
	       (unique-triplets n))))

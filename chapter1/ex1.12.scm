; computes n'th pascal triangle number
(define (pascal n)
  (if (= (row n 1) 1)
    1
    (+ (pascal (- n (row n 1)))
       (pascal (- n (- (row n 1) 1))))))

; returns 1 if n'th position is on edge
(define (row n i)
  (cond ((or (= n 1) (= n i)) 1)
	((< n i) i)
	(else (row (- n i) (+ i 1)))))

(define (pascal-triangle row column)
  (if (or (= column 1) (= row column))
    1
    (+ (pascal-triangle (- row 1) (- column 1))
       (pascal-triangle (- row 1) column))))

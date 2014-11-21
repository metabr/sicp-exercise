;; exercise 2.3
;; rectangle representation

(define (square x) (* x x))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (segment-length s)
  (let ((x1 (x-point (start-segment s)))
	(x2 (x-point (end-segment s)))
	(y1 (y-point (start-segment s)))
	(y2 (y-point (end-segment s))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

;; rectangle defined as length and width parts, constructed from
;; line segments

(define (make-rect l w)
  (cons (segment-length l) (segment-length w)))

(define (l r) (car r))

(define (w r) (cdr r))

(define (rect-area r)
  (* (l r) (w r)))

(define (rect-perimeter r)
  (+ (* 2 (l r)) (* 2 (w r))))

(define my-rect (make-rect (make-segment (make-point 0 0) (make-point 0 10))
			   (make-segment (make-point 0 0) (make-point 4 0))))

(newline)
(display "Area of my-rect: ")
(display (rect-area my-rect))
(newline)
(display "Perimeter of my-rect: ")
(display (rect-perimeter my-rect))

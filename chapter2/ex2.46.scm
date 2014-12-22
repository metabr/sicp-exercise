(define (make-vect x y)
  (cons x y))

(define (xcor vect)
  (car vect))

(define (ycor vect)
  (cdr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
	     (+ (ycor v1) (ycor v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor v1) (xcor v2))
	     (- (ycor v1) (ycor v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor v))
	     (* s (ycor v))))

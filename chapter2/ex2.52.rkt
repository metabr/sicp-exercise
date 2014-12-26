#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (wave frame)
  ((segments->painter (list (make-segment (make-vect 0.4 0) (make-vect 0.5 0.28))
			    (make-segment (make-vect 0.5 0.28) (make-vect 0.6 0))
			    (make-segment (make-vect 0.744 0) (make-vect 0.6 0.44))
			    (make-segment (make-vect 0.6 0.44) (make-vect 1 0.136))
			    (make-segment (make-vect 1 0.36) (make-vect 0.736 0.64))
			    (make-segment (make-vect 0.736 0.64) (make-vect 0.6 0.64))
			    (make-segment (make-vect 0.6 0.64) (make-vect 0.656 0.84))
			    (make-segment (make-vect 0.656 0.84) (make-vect 0.6 1))
			    (make-segment (make-vect 0.4 1) (make-vect 0.344 0.84))
			    (make-segment (make-vect 0.344 0.84) (make-vect 0.4 0.64))
			    (make-segment (make-vect 0.4 0.64) (make-vect 0.296 0.64))
			    (make-segment (make-vect 0.296 0.64) (make-vect 0.136 0.576))
			    (make-segment (make-vect 0.136 0.576) (make-vect 0 0.864))
			    (make-segment (make-vect 0 0.640) (make-vect 0.136 0.336))
			    (make-segment (make-vect 0.136 0.336) (make-vect 0.296 0.58))
			    (make-segment (make-vect 0.296 0.58) (make-vect 0.336 0.48))
			    (make-segment (make-vect 0.336 0.48) (make-vect 0.256 0))
			    (make-segment (make-vect 0.55 0.72) (make-vect 0.45 0.72)) ;-|
			    )
		      )
   frame))

(paint wave)

(define (split a b)
  (define (my-split painter n)
    (if (= n 0)
      painter
      (let ((smaller (my-split painter (- n 1))))
	(a painter (b smaller smaller)))))
  my-split)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

(paint (corner-split wave 2))

(define (my-corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1)))
	  (corner (my-corner-split painter (- n 1))))
      (beside (below painter up)
	      (below right corner)))))

(paint (my-corner-split wave 2))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 2))

(define (my-square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
				  identity flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (my-square-limit einstein 2))

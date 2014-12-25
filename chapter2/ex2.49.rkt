#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define origin (make-vect 0 0))
(define lr (make-vect 0.99 0))
(define ul (make-vect 0 0.99))
(define ur (make-vect 0.99 0.99))

; a. The painter that draws the outline of designated frame.
(define (outline frame) 
  ((segments->painter (list (make-segment origin lr)
			    (make-segment lr ur)
			    (make-segment ur ul)
			    (make-segment ul origin)))
   frame))

(paint outline)

; b. The painter that draws an "X" by connecting opposite corners of frame.
(define (x frame)
  ((segments->painter (list (make-segment origin ur)
			    (make-segment ul lr)))
   frame))

(paint x)

; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define (diamond frame)
  ((segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                            (make-segment (make-vect 0.5 0) (make-vect 0 0.5))))
   frame))

(paint diamond)

; d. The wave painter.
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
			    (make-segment (make-vect 0.136 0.576) (make-vect 0 0.864)) ;
			    (make-segment (make-vect 0 0.640) (make-vect 0.136 0.336)) ;
			    (make-segment (make-vect 0.136 0.336) (make-vect 0.296 0.58))
			    (make-segment (make-vect 0.296 0.58) (make-vect 0.336 0.48))
			    (make-segment (make-vect 0.336 0.48) (make-vect 0.256 0))
			    )
		      )
   frame))

(paint wave)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(paint wave2)
(paint wave4)

; Testing with procedures from 2.44.
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

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(paint (corner-split outline 7))
(paint (corner-split x 7))
(paint (corner-split diamond 7))
(paint (up-split x 10))

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

; d. The wave painter.

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
(paint (up-split x 10))

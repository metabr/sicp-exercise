(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
	      (choose-branch (car bits) current-branch)))
	(if (leaf? next-branch)
	  (cons (symbol-leaf next-branch)
		(decode-1 (cdr bits) tree))
	  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-found? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (symbol-found? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (lookup s tree result)
    (cond ((symbol-found? s (symbols (left-branch tree)))
	   (if (leaf? (left-branch tree))
	     (append result '(0))
	     (lookup s (left-branch tree) (append result '(0)))))
	  ((symbol-found? s (symbols (right-branch tree)))
	   (if (leaf? (right-branch tree))
	     (append result '(1))
	     (lookup s (right-branch tree) (append result '(1)))))
	  (else (if (null? result)
		  (error "no symbol in tree -- ENCODE-SYMBOL" symbol)
		  result))))
  (lookup symbol tree '()))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		    (make-leaf 'B 2)
		    (make-code-tree (make-leaf 'D 1)
				    (make-leaf 'C 1)))))

(define sample-message '(a d a b c a))

(newline)
(display "Encoded message: ")
(display (encode sample-message sample-tree))
(newline)
(display "Encoding message with symbol not from tree:")
(newline)
(display (encode '(a f d) sample-tree))

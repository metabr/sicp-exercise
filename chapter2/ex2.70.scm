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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
	((null? (cdr leaf-set)) (car leaf-set))
	(else (successive-merge (adjoin-set (make-code-tree (car leaf-set)
							    (cadr leaf-set))
					    (cddr leaf-set))))))

(define alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define tree (generate-huffman-tree alphabet))
(define message '(Get a job
		  Sha na na na na na na na na
		  Get a job
		  Sha na na na na na na na na
		  Wah yip yip yip yip yip yip yip yip yip
		  Sha boom))
(define encoded-message (encode message tree))

(newline)
(newline)
(display "Huffman tree: ")
(display tree)
(newline)
(display "Encoded message: ")
(display encoded-message)
(newline)
(display "Decoded: ")
(display (decode encoded-message tree))

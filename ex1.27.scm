(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael-test n)
  (carmichael-iter n 1))

(define (carmichael-iter n a)
  (if (= a n) true
    (if (= (expmod a n n) a)
      (carmichael-iter n (+ a 1))
      false)))

(define (print-test n)
  (newline)
  (display n)
  (display " - Carmichael: ")
  (display (carmichael-test n)))

(print-test 561)
(print-test 1105)
(print-test 1729)
(print-test 2465)
(print-test 2821)
(print-test 6601)

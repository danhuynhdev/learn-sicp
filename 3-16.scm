(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))
(define p3real '(1 2 3))
(define p '(3))
(define p3 (cons p p))
(define p4 (cons (cons 2 p) p))
(define p7 (cons p3 p3))
(define (mystery x)
  (define (loop x y)
	(if (null? x)
		y
		(let ((temp (cdr x)))
		  (set-cdr! x y)
		  (loop temp x))))
  (loop x '()))
(define ptoinfinityandbeyond (mystery '(1 2 3)))
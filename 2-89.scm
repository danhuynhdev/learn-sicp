(define (adjoin-term-dense term term-list)
  (if (=zero? (coeff term))
	  term-list
	  (cond ((= (order term) (length term-list))
			 (cons (coeff term) term-list))
			((> (order term) (length term-list))
			 (adjoin-term-dense term (cons 0 term-list))))))

(define (first-term-dense term-list)
  (make-term (car term-list) (- (length term-list) 1)))

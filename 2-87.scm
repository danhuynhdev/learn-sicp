(define (install-polynomial-package)
  ;; ....
  (put '=zero? 'polynomial
	   (lambda (p) true))
  ;; ....
  'done)
(define (install-raise-package)
  (put 'raise 'integer
	   (lambda (x) (make-rational x 1)))
  (put 'raise 'rational
	   (lambda (x) (make-real (/ (numer x) (denom y)))))
  (put 'raise 'real
	   (lambda (x) (make-from-real-imag x 0))))

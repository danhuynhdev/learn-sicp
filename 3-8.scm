(define f
  (let ((run? #f))
	(lambda (x)
	  (if run?
		  0
		  (begin
			(set! run? #t)
			x)))))

;;; f should be reset each test.

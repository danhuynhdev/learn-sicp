(define (install-project-package)
  (put 'project 'complex
	   (lambda (x) (make-real (real-part x))))
  (put 'project 'real
	   (lambda (x)
		 (let ((exact (inexact->exact x)))
		   (make-rational (numerator x)
						  (denominator x)))))
  (put 'project 'rational
	   (lambda (x) (numer x))))

(define (drop x)
  (let ((project (get 'project (type-tag x))))
	(if project
		(let ((y (project (content x))))
		  (if (equ? x (raise (content y)))
			  (drop y)
			  x))
		x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (drop (apply proc (map contents args)))
		  (if (= (length args) 2)
			  (let ((a1 (car args))
					(a2 (cadr args)))
				(cond ((< (level a1) (level a2))
					   (apply-generic op a1 ((get 'raise (type-tag a2)) a2)))
					  ((> (level a1) (level a2))
					   (apply-generic op ((get 'raise (type-tag a1)) a1) a2))
					  (else (error "No method for these types"
								   (list op type-tags)))))
			  (error "No method for these types"
					 (list op type-tags)))))))

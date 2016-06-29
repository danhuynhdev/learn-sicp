(define (level x)
  (let ((raise (get 'raise (type-tag x))))
	(if raise
		(+ 1
		   (level (raise (content x))))
		0)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (if proc
		  (apply proc (map contents args))
		  (apply proc (map contents (coersion args type-tags)))))))

(define (coersion args type-tags)
  (if (null? type-tags)
	  (error "No method for these types"
			 (list op type-tags))
	  (let ((type (car type-tags)))
		(if (can-coerce? type-tags type)
			(map (lambda (arg)
				   (let (coerce (get-coercion (type-tag arg) type))
					 (if coerce
						 (coerce arg)
						 arg)))
				 args)
			(coersion args (cdr type-tags))))))

(define (can-coerce? types des-type)
  (accumulate (lambda (type can?)
				(and can?
					 (or (equal? type des-type)
						 (get-coercion type des-type))))
			  #t
			  types))


;;; It will be insufficient in the case we have coersion like this
;;; a->b b->c
;;; a can be coerce to c but have to be coerce to b first and the procedure
;;; above do not cover this case.

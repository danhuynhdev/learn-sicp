(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
				 (add-terms (term-list p1)
							(negate-poly p2)))
	  (error "Polys not in same var: ADD-POLY" (list p1 p2))))


(define (negate-poly p)
  (map (lambda (t)
		 (if (pair? (coeff t))
			 (negate-poly (coeff t))
			 (make-term (order t) (- (coeff t)))))
	   (term-list p)))

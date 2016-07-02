(define rand
  (let ((seed  0))
	(define (reset s)
	  (set! seed s))
	(lambda (m)
	  (cond ((equal? m 'generate)
			 (set! seed (1+ seed))
			 seed)
			((equal? m 'reset) reset)))))

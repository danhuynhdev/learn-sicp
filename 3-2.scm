(define (make-monitored f)
  (let ((count 0))
	(lambda (mes)
	  (cond ((equal? mes 'how-many-calls?) count)
			((equal? mes 'reset-count) (set! count 0))
			(else (set! count (1+ count))
				  (f mes))))))

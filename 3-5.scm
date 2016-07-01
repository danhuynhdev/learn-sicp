(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1)
				 (+ trials-passed 1)))
		  (else
		   (iter (- trials-remaining 1)
				 trials-passed))))
  (iter trials 0))

(define (square x) (* x x))

(define (in-circle? cx cy r)
  (lambda (x y)
	(<= (+ (square (- x cx))
		   (square (- y cy)))
		(square r))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (lambda () (P (random-in-range x1 x2)
									(random-in-range y1 y2)))))

(define (estimate-pi trials)
  (/ (* (estimate-integral (in-circle? 5 7 3)
						   2 8 4 10 trials)
		(* 6 6))
	 9.0))

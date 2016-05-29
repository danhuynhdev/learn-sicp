#lang sicp

; 1.3
(define (square x)
  (* x x))

(define (sum-square a b)
  (+ (square a) (square b)))

(define (sum-square-larger a b c)
  (cond ((and (> a c) (> b c)) (sum-square a b))
		((and (> b a) (> c a)) (sum-square b c))
		(else (sum-square a c))))

; 1.5
; On an environment use applicative-orde evaluation like lisp's environment
; the test function will produce an infinite loop because lisp try to evaluate
; the arguments before apply the test procedure to the arguments.
; On an environment use normal-order evaluation i think the test procedure will
; run fine because the if procedure only evaluate then-clause or else-clause if
; needed after evaluate predicate.
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Ex 1.1.7 Square root by newton's method
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- x
			 (square guess)))
	 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; 1.6:
; Because lisp follow the aplicative-order
; the program will not respect the if rule and will
; evaluate all arguments before apply the procedure to the arguments
; and run the recursive call no matter if good-enough? return #t or #f
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
		  guess
		  (sqrt-iter (improve guess x) x)))

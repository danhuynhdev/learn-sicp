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

;;; 1.5
;;; On an environment use applicative-orde evaluation like lisp's environment
;;; the test function will produce an infinite loop because lisp try to evaluate
;;; the arguments before apply the test procedure to the arguments.
;;; On an environment use normal-order evaluation i think the test procedure will
;;; run fine because the if procedure only evaluate then-clause or else-clause if
;;; needed after evaluate predicate.

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

;;; 1.6:
;;; Because lisp follow the aplicative-order
;;; the program will not respect the if rule and will
;;; evaluate all arguments before apply the procedure to the arguments
;;; and run the recursive call no matter if good-enough? return #t or #f

;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;; 		(else else-clause)))

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;; 		  guess
;; 		  (sqrt-iter (improve guess x) x)))

;;; 1.7
;;; For small numbers the use of 0.001 as biggest diff is not ok because the square
;;; of guess can be smaller than 0.001. Ex: (sqrt 0.00000001) should be 0.0001 but
;;; become 0.031250106... instead.
;;; For big number i still don't know why but i guess it is because math on big
;;; inexact number is slow and may be can't produce diff smaller than 0.001.
;;; Ex: (sqrt 1E13) with diff 0.001 make an infinite loop but (sqrt 1E13) with diff
;;; 0.01 -> 3162277.660...

;; (define (good-enough? guess x)
;;   (< (abs (- guess
;; 			 (improve guess x)))
;; 	 (/ guess 1000000)))

;;; 1.8
(define (cube-root x)
  (define (improve guess)
	(/ (+ (/ x (square guess))
		  (* 2 guess))
	   3))
  (define (good-enough? guess)
	(< (abs (- guess
			   (improve guess)))
	   (/ guess 1000000)))
  (define (iter guess)
	(if (good-enough? guess x)
		guess
		(iter (improve guess x) x)))
  (iter 1.0))

;;; 1.9

;; (define (+a b)
;;   (if (=a 0) b (inc (+ (dec a) b))))

;;; (+ 4 5)
;;; (inc (+ 3 5))
;;; (inc (inc (+ 2 5)))
;;; (inc (inc (inc (+ 1 5))))
;;; (inc (inc (inc (inc (+ 0 5)))))
;;; (inc (inc (inc (inc 5))))
;;; (inc (inc (inc 6)))
;;; (inc (inc 7))
;;; (inc 8)
;;; 9

;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b))))

;;; (+ 4 5)
;;; (+ 3 6)
;;; (+ 2 7)
;;; (+ 1 8)
;;; (+ 0 9)
;;; 9

;;; procedure 1 is recursive. procedure 2 is iterative

;;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1) (A x (- y 1))))))

;;; (A 1 10) -> 2 ** 10
;;; (A 2 4) -> 2 ** 16
;;; (A 3 3) -> 2 ** 16

;;; (define (f n) (A 0 n)) -> 2 * n
;;; (define (g n) (A 1 n)) -> 2 ** n
;;; (define (h n) (A 2 n)) -> 2 ** 2 ** 2 ** ... With n numbers 2

;;; Fibonacci

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
	  b
	  (fib-iter (+ a b) a (dec count))))

;;; Count change

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount
					 (- kinds-of-coins 1))
				 (cc (- amount
						(first-denomination
						 kinds-of-coins))
					 kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

;;; 1.11
(define (compute-f-recursive n)
  (if (< n 3)
	  n
	  (+ (compute-f-recursive (- n 1))
		 (* 2
			(compute-f-recursive (- n 2)))
		 (* 3
			(compute-f-recursive (- n 3))))))

(define (compute-f n)
  (define (iter f+2 f+1 f n)
	(if (= n 0)
		f
		(iter (+ f+2
				 (* 2 f+1)
				 (* 3 f))
			  f+2
			  f+1
			  (dec n))))
  (iter 2 1 0 n))

;;; 1.12

(define (pascal-triangle row col)
  (cond ((or (> col row)
			 (< col 0)) 0)
		((= col 0) 1)
		(else (+ (pascal-triangle (dec row) (dec col))
				 (pascal-triangle (dec row) col)))))

(define (pascal height)
  (define (iter row col height)
	(if (> height 0)
		(if (> col row)
			(begin
			  (display "\n")
			  (iter (+ row 1) 0 (- height 1)))
			(begin
			  (display (pascal-triangle row col))
			  (display " ")
			  (iter row (+ col 1) height)))))
  (iter 0 0 height))

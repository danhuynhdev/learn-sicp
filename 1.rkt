#lang planet neil/sicp

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

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0) 0 y))

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

;; (define (sqrt x)
;;   (sqrt-iter 1.0 x))

;;; 1.6:
;;; Because lisp follow the aplicative-order
;;; the program will not respect the if rule and will
;;; evaluate all arguments before apply the procedure to the arguments
;;; and run the recursive call no matter if good-enough? return true or false

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
;; (define (cube-root x)
;;   (define (improve guess)
;; 	(/ (+ (/ x (square guess))
;; 		  (* 2 guess))
;; 	   3))
;;   (define (good-enough? guess)
;; 	(< (abs (- guess
;; 			   (improve guess)))
;; 	   (/ guess 1000000)))
;;   (define (iter guess)
;; 	(if (good-enough? guess x)
;; 		guess
;; 		(iter (improve guess x) x)))
;;   (iter 1.0))

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

;;; 1.14
;;; 10 5 -> 10-50 5 -> 0
;;; `- 10 4 -> 10-25 4 -> 0
;;;     `-> 10 3 -> 10-10 3 -> 1
;;;          `-> 10 2 -> 10-5 2 -> 5-5 2 -> 1
;;;              \        `-> 5 1 ->5-1 1 ->4-1 1 ->3-1 1 -> 2-1 1 -> 1-1 1 -> 0 1 -> 1
;;;               10 1        `>0   `> 0     `>0      `> 0     `> 0      `>0      `> 0
;;;                `> 10 ... -> 1
;;;                   `-> 0
;;; Ans -> 4
;;; O(n^k) with k = kinds of coins don't need much explaination i just read it
;;; from a article :p like those mathetical stuff. I wanna learn fp not math.

;;; 1.15

;; (define (cube x) (* x x x))
;; (define (p x) (- (* 3 x) (* 4 (cube x))))
;; (define (sine angle)
;;   (if (not (> (abs angle) 0.1))
;; 	  angle
;; 	  (p (sine (/ angle 3.0)))))

;;; 5 (/ 12.05 3 3 3 3 3) -> 0.0499.. < 0.1
;;; i think it is O(log(n))

;;; Exponentiation

;; (define (expt b n)
;;   (if (= n 0)
;; 	  1
;; 	  (* b (expt b (- n 1)))))

;; (define (expt b n)
;;   (expt-iter b n 1))
;; (define (expt-iter b counter product)
;;   (if (= counter 0)
;; 	  product
;; 	  (expt-iter b
;; 				 (- counter 1)
;; 				 (* b product))))

;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;; 		((even? n) (square (fast-expt b (/ n 2))))
;; 		(else (* b (fast-expt b (- n 1))))))

;; (define (even? n)
;;   (= (remainder n 2) 0))

;;; 1.16

;; (define (fast-expt b n)
;;   (fast-expt-iter b n 1))

;; (define (fast-expt-iter b n a)
;;   (cond ((= n 0) a)
;; 		((even? n) (fast-expt-iter (square b) (/ n 2) a))
;; 		(else (fast-expt-iter b (- n 1) (* a b)))))

;;; 1.17
;;; a*b = a*2 * (b/2) if b is even
;;; a*b = a*(b-1) + a if b is odd

;;; 1.18

;; (define (double x) (+ x x))
;; (define (halve x) (/ x 2))

;; (define (* a b)
;;   (define (iter a b n)
;; 	(cond ((= b 0) n)
;; 		  ((even? b) (iter (double a) (halve b) n))
;; 		  (else (iter a (- b 1) (+ n a)))))
;;   (iter a b 0))

;;; 1.19

(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))
(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
		((even? count)
		 (fast-fib-iter a
						b
						(+ (square q) (square p))
						(+ (* 2 q p) (square q))
						(/ count 2)))
		(else (fast-fib-iter (+ (* b q) (* a q) (* a p))
							 (+ (* b p) (* a q))
							 p
							 q
							 (- count 1)))))

;;; GCD

;; (define (gcd a b)
;;   (if (= b 0)
;; 	  a
;; 	  (gcd b (remainder a b))))

;;; 1.20
;;; (gcd 206 40) 1 -> (gcd 40 6) 1 -> (gcd 6 4) 1 -> (gcd 4 2) 1 -> (gcd 2 0) -> 2
;;; applicative-order: 4
;;; normal-order: a lot. Such a waste of time to count.

;;; Searching for divisors

(define (smallest-divisor n) (find-divisor n 2))

;;; test test-divisor from 2 to (sqrt n) till it devides b. If can't find divisor then
;;; return n.

;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;; 		((divides? test-divisor n) test-divisor)
;; 		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; O(log(n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
		((even? exp)
		 (remainder
		  (square (expmod base (/ exp 2) m))
		  m))
		(else
		 (remainder
		  (* base (expmod base (- exp 1) m))
		  m))))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

;;; 1.21

;; (smallest-divisor 199) -> 199
;; (smallest-divisor 1999) -> 1999
;; (smallest-divisor 19999) -> 7

;;; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;; (define (start-prime-test n start-time)
;;   (if (prime? n)
;; 	  (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (define (iter n count)
	(if (< count 3)
		(if (and (prime? n) (timed-prime-test n))
			(iter (+ n 2) (+ count 1))
			(iter (+ n 2) count))))
  (iter (+ n 1) 0))

;;; (search-for-primes 1000)
;;; 1009 *** 6
;;; 1013 *** 4
;;; 1019 *** 5
;;; (search-for-primes 10000)
;;; 10007 *** 16
;;; 10009 *** 12
;;; 10037 *** 14
;;; (search-for-primes 100000)
;;; 100003 *** 42
;;; 100019 *** 36
;;; 100043 *** 35
;;; (search-for-primes 1000000)
;;; 1000003 *** 114
;;; 1000033 *** 110
;;; 1000037 *** 111

;;; I think the result is pretty ok. For example 1013 compute in 4 microseconds
;;; and 10009 compute in 12 microseconds exactly equal (* 4 (sqrt 10)).

;;; 1.23

(define (next x)
  (if (= x 2)
	  3
	  (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))

;;; It is only faster for big number like 100000. Can't see any different with
;;; number smaller than that.

;;; 1.24

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
	  (report-prime (- (runtime) start-time))))

;;; (search-for-primes 1000)
;;; 1009 *** 43
;;; 1013 *** 25
;;; 1019 *** 40
;;; (search-for-primes 10000)
;;; 10007 *** 35
;;; 10009 *** 29
;;; 10037 *** 29
;;; (search-for-primes 100000)
;;; 100003 *** 46
;;; 100019 *** 41
;;; 100043 *** 42
;;; (search-for-primes 1000000)
;;; 100003 *** 42
;;; 100019 *** 34
;;; 100043 *** 35

;;; The time actually longer with small numbers but they grow much much slower
;;; with bigger numbers with a massive different with 1000000.

;;; 1.25
;;; The result will stay the same but if we use her way it will take a lot longer
;;; to compute because fast-expt will produce a very big number and doing math
;;; stuff on big numbers is very expensive to the machines.

;;; 1.26
;;; When she do that because lisp follow applicative-order it will execute expmod
;;; twice instead of once like in the original procedure.

;;; 1.27
;; (define (carmichael-test n)
;;   (define (iter a pass?)
;; 	(cond ((= a 0) true)
;; 		  (pass? (iter (- a 1) (= (expmod a n n) a)))
;; 		  (else false)))
;;   (iter (- n 1) true))

;; (carmichael-test 561)
;; (carmichael-test 1105)
;; (carmichael-test 1729)

;;; 1.28
;;; #TODO

;;; Procedures as arguments

;; (define (sum term a next b)
;;   (if (> a b)
;; 	  0
;; 	  (+ (term a)
;; 		 (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum (lambda(x) x) a inc b))

(define (pi-sum a b)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	(+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
	(+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	 dx))

;;; 1.29

(define (better-intergral f a b n)
  (let ((h (/ (- b a) n)))
	(define (next k)
	  (let ((yk (f (+ a (* k h)))))
		(cond ((or (= k 0) (= k n)) yk)
			  ((even? k) (* 2 yk))
			  (else (* 4 yk)))))
	(* (sum next 0 inc n)
	   (/ h 3))))

;;; The results is much more accurate than the old one.

;;; 1.30

(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

;;; 1.31

(define (product term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (* result (term a)))))
  (iter a 1))

(define (rec-product term a next b)
  (if (> a b)
	  1
	  (* (term a)
		 (rec-product term (next a) next b))))

(define (quarter-pi n)
  (define (next x)
	(if (even? x)
		(/ (+ 2 x)
		   (+ 3 x))
		(/ (+ 2 (* 2 (+ (quotient x 2) 1)))
		   (+ 3 (- x 1)))))
  (product next 0.0 inc n))

;;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (rec-accumulate combiner null-value term a next b)
  (if (> a b)
	  null-value
	  (combiner (term a)
				(rec-accumulate combiner
								null-value
								term
								(next a)
								next
								b))))

;;; 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a)
			  (combiner result
						(if (filter a)
							(term a)
							null-value)))))
  (iter a null-value))

(define (sum-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-b n)
  (define (filter i)
	(and (< i n)
		 (= (gcd i n) 1)))
  (filtered-accumulate * 1 (lambda(a) a) 1 inc n filter))

;;; 1.34
;;; It will say 2 is not a procedure
;;; It will evaluate like this:
;;; (f f) -> (f 2) -> (2 2)

;;; Procedures as General methods

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
		midpoint
		(let ((test-value (f midpoint)))
		  (cond ((positive? test-value)
				 (search f neg-point midpoint))
				((negative? test-value)
				 (search f midpoint pos-point))
				(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
		   (error "Values are not of opposite sign" a b)))))

;;; Finding fixed points of functions

(define tolerance 0.00001)
;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;; 	(< (abs (- v1 v2))
;; 	   tolerance))
;;   (define (try guess)
;; 	(let ((next (f guess)))
;; 	  (if (close-enough? guess next)
;; 		  next
;; 		  (try next))))
;;   (try first-guess))

;; (define (sqrt x)
;;   (fixed-point (lambda (y) (average y (/ x y)))
;; 			   1.0))

;;; 1.35

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ x)))
			   1.0))

;;; 1.36

;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;; 	(< (abs (- v1 v2))
;; 	   tolerance))
;;   (define (try guess)
;; 	(display guess) (newline)
;; 	(let ((next (f guess)))
;; 	  (if (close-enough? guess next)
;; 		  next
;; 		  (try next))))
;;   (try first-guess))

;;; This take 36 steps
(define (sol-1.36)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
			   2.0))

;;; This take 10 steps
(define (sol-1.36-damped)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x)))
							  2))
			   2.0))

;;; 1.37

(define (cont-frac n d k)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (dec i) (/ (n i)
						 (+ (d i) result)))))
  (iter k 0))

;;; k needs to be equal 11 to get an approximation that is accurate to 4 decimal places

(define (rec-cont-frac n d k)
  (define (iter i)
	(if (> i k)
		0
		(/ (n i)
		   (+ (d i) (iter (inc i))))))
  (iter 1))

;;; 1.38

(define (e-2 k)
  (cont-frac (lambda (i) 1.0)
			 (lambda (i) (if (= (modulo (- i 2) 3) 0)
							 (* 2 (/ (+ i 1) 3))
							 1.0))
			 k))

;;; 1.39

(define (tangent x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
			 (lambda (i) (- (* 2 i) 1))
			 k))

;;; Procedures as returned values

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; (define (sqrt x)
;;   (fixed-point (average-damp (lambda (y) (/ x y)))
;; 			   1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
			   1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; (define (sqrt x)
;;   (newtons-method
;;    (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; (define (sqrt x)
;;   (fixed-point-of-transform
;;    (lambda (y) (/ x y)) average-damp 1.0))

;; (define (sqrt x)
;;   (fixed-point-of-transform
;;    (lambda (y) (- (square y) x)) newton-transform 1.0))

;;; 1.40

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;;; 1.41

(define (double f)
  (lambda (x) (f (f x))))

;;; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;;; 1.43

(define (repeat f n)
  (lambda (x)
	(define (iter i result)
	  (if (= i n)
		  result
		  (iter (inc i) (f result))))
	(iter 0 x)))

;;; 1.44

(define (smooth f)
  (lambda (x) (/ (f (- x dx) (f x) (f (+ x dx)))
				 3)))

(define (n-fold f n)
  ((repeat smooth n) f))

;;; 1.45

(define (nth-root x n)
   (fixed-point
	((repeat average-damp (- n 2)) (lambda (y) (/ x (expt y (- n 1)))))
	1.0))

;;; 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
	(define (iter guess)
	  (let ((next (improve guess)))
		(if (good-enough? guess next)
			next
			(iter next))))
	(iter guess)))

(define (sqrt x)
  ((iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 1.0))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))


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

(define (gcd a b)
  (if (= b 0)
	  a
	  (gcd b (remainder a b))))

;;; 1.20
;;; (gcd 206 40) 1 -> (gcd 40 6) 1 -> (gcd 6 4) 1 -> (gcd 4 2) 1 -> (gcd 2 0) -> 2
;;; applicative-order: 4
;;; normal-order: a lot. Such a waste of time to count.

;;; Searching for divisors

(define (smallest-divisor n) (find-divisor n 2))

;;; test test-divisor from 2 to (sqrt n) till it devides b. If can't find divisor then
;;; return n.

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

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
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

;;; 1.21

;; (smallest-divisor 199) -> 199
;; (smallest-divisor 1999) -> 1999
;; (smallest-divisor 19999) -> 7

;;; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
	  (report-prime (- (runtime) start-time))))
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

;;; I think the result is pretty ok. For example 1013 compute in 4 microseconds
;;; and 10009 compute in 12 microseconds exactly equal (* 4 (sqrt 10)).

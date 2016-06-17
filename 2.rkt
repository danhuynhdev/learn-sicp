#lang planet neil/sicp

(define (average x y)
  (/ (+ x y) 2))
;;; Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

;; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; 2.1

(define (make-rat n d)
  (let ((gc-div (gcd n d)))
	(let ((n (/ n gc-div))
		  (d (/ d gc-div)))
	  (if (and (positive? n) (negative? d))
		  (cons (- n) (- d))
		  (cons n d)))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;;; 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
					   (x-point (end-segment seg)))
			  (average (y-point (start-segment seg))
					   (y-point (end-segment seg)))))

;;; 2.3

(define (rec-perimeter rec)
  (* (+ (rec-width rec)
		(rec-height rec))
	 2))

(define (rec-area rec)
  (* (rec-width rec)
	 (rec-height rec)))

;; (define (make-rec top-left top-right height)
;;   (cons top-left (cons width height)))

;; (define (rec-height rec) (cdr (cdr rec)))

;; (define (rec-width rec) (car (cdr rec)))

(define (make-rec top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rec) (car rec))

(define (top-right rec)
  (make-point (x-point (bottom-right rec))
			  (y-point (top-left rec))))

(define (bottom-left rec)
  (make-point (x-point (top-left rec))
			  (y-point (bottom-right rec))))

(define (bottom-right rec)
  (cdr rec))

(define (rec-height rec)
  (- (y-point (top-left rec))
	 (y-point (bottom-left rec))))

(define (rec-width rec)
  (- (x-point (top-right rec))
	 (x-point (top-left rec))))

;; (define (cons x y)
;;   (define (dispatch m)
;; 	(cond ((= m 0) x)
;; 		  ((= m 1) y)
;; 		  (else (error "Argument not 0 or 1: CONS" m))))
;;   dispatch)
;; (define (car z) (z 0))
;; (define (cdr z) (z 1))

;;; 2.4

;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))

;;; 2.5

(define (cons-i a b)
  (* (expt 2 a) (expt 3 b)))
(define (car-i p)
  (define (iter p i)
	(if (not (= (modulo p 2) 0))
		i
		(iter (/ p 2) (inc i))))
  (iter p 0))
(define (cdr-i p)
  (define (iter p i)
	(if (not (= (modulo p 3) 0))
		i
		(iter (/ p 3) (inc i))))
  (iter p 0))

;;; 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f)
	(lambda (x)
	  ((a f) ((b f) x)))))

;;; Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

;; (define (div-interval x y)
;;   (mul-interval
;;    x
;;    (make-interval (/ 1.0 (upper-bound y))
;; 				  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;;; 2.7

(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (cdr i) (cdr i)))

;;; 2.8

(define (sub-interval x y)
  (add-interval x
				(make-interval (- (upper-bound y))
							   (- (lower-bound y)))))

;;; 2.9

;; (define (width i)
;;   (/ (- (upper-bound i) (lower-bound i)) 2))

;;; This is some basic math
;;; (width (add-interval a b)) ->
;;; (/ (- (+ (lower-bound a) (lower-bound b))
;;;       (+ (upper-bound a) (upper-bound b)))
;;;    2) ->
;;; (- (/ (+ (lower-bound a) (lower-bound b))
;;;       2)
;;;    (/ (+ (upper-bound a) (upper-bound b))
;;;       2)) ->
;;; (- (+ (lower-bound (/ a 2)) (lower-bound (/ b 2)))
;;;    (+ (upper-bound (/ a 2)) (upper-bound (/ b 2))))

;;; 2.10

(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
	  (error "Divide by an interval that spans zero")
	  (mul-interval
	   x
	   (make-interval (/ 1.0 (upper-bound y))
					  (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;; 2.12

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100.)))
	(make-center-width c w)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;;; 2.13
;;; math stuff don't care :|


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
				(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
	(div-interval
	 one (add-interval (div-interval one r1)
					   (div-interval one r2)))))

;;; 2.14

(define r1 (make-center-percent 10 1))
(define r2 (make-center-percent 20 1))

;; (par1 r1 r2) --> [6.4693..., 6.8693...]
;; (par2 r1 r2) --> [6.6, 6.733...]


;;; Hierarchical data and the closure property

;;; 2.17

(define (last-pair list)
  (if (null? (cdr list))
	  (car list)
	  (last-pair (cdr list))))

;;; 2.18

(define (reverse list)
  (define (iter r l)
	(if (null? l)
		r
		(iter (cons (car l) r) (cdr l))))
  (iter (cons (car list) nil) (cdr list)))

;;; 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
		 (+ (cc amount
				(except-first-denomination
				 coin-values))
			(cc (- amount
				   (first-denomination
					coin-values))
				coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

;;; The order with not affect the answer because it does not affect the combinations of coins

;;; 2.20

(define (same-parity f . ints)
  (define (iter ints)
	(cond ((null? ints) ints)
		  (((if (even? f) even? odd?) (car ints)) (cons (car ints)
														(iter (cdr ints))))
		  (else (iter (cdr ints)))))
  (cons f (iter ints)))

;;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
	  nil
	  (cons (* (car items) factor)
			(scale-list (cdr items)
						factor))))

(define (scale-list-map items factor)
  (map (lambda (x) (* x factor))
	   items))

;;; 2.21

(define square (lambda (x) (* x x)))
(define (square-list items)
  (if (null? items)
	  nil
	  (cons (square (car items)) (square-list (cdr items)))))
(define (square-list-map items)
  (map square items))

;;; 2.22
;;; Because we can only iterate from begin to start, but to construct the list we have to construct from the end.

;;; 2.23

;; (define (for-each proc items)
;;   (if (not (null? items))
;; 	  (begin
;; 		(proc (car items))
;; 		(for-each proc (cdr items)))))

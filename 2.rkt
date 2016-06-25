#lang sicp
(#%require sicp-pict)

(define (average x y)
  (/ (+ x y) 2))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
	  b
	  (fib-iter (+ a b) a (dec count))))

(define (find-divisor n test-divisor)
  (define (next x)
	(if (= x 2)
		3
		(+ x 2)))
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
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

;; (define (print-point p)
;;   (newline)
;;   (display "(")
;;   (display (x-point p))
;;   (display ",")
;;   (display (y-point p))
;;   (display ")"))

;; (define make-segment cons)
;; (define start-segment car)
;; (define end-segment cdr)

;; (define make-point cons)
;; (define x-point car)
;; (define y-point cdr)

;; (define (midpoint-segment seg)
;;   (make-point (average (x-point (start-segment seg))
;; 					   (x-point (end-segment seg)))
;; 			  (average (y-point (start-segment seg))
;; 					   (y-point (end-segment seg)))))

;;; 2.3

;; (define (rec-perimeter rec)
;;   (* (+ (rec-width rec)
;; 		(rec-height rec))
;; 	 2))

;; (define (rec-area rec)
;;   (* (rec-width rec)
;; 	 (rec-height rec)))

;; (define (make-rec top-left top-right height)
;;   (cons top-left (cons width height)))

;; (define (rec-height rec) (cdr (cdr rec)))

;; (define (rec-width rec) (car (cdr rec)))

;; (define (make-rec top-left bottom-right)
;;   (cons top-left bottom-right))

;; (define (top-left rec) (car rec))

;; (define (top-right rec)
;;   (make-point (x-point (bottom-right rec))
;; 			  (y-point (top-left rec))))

;; (define (bottom-left rec)
;;   (make-point (x-point (top-left rec))
;; 			  (y-point (bottom-right rec))))

;; (define (bottom-right rec)
;;   (cdr rec))

;; (define (rec-height rec)
;;   (- (y-point (top-left rec))
;; 	 (y-point (bottom-left rec))))

;; (define (rec-width rec)
;;   (- (x-point (top-right rec))
;; 	 (x-point (top-left rec))))

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
	  (display "Divide by an interval that spans zero")
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

;; (define (count-leaves x)
;;   (cond ((null? x) 0)
;; 		((not (pair? x)) 1)
;; 		(else (+ (count-leaves (car x))
;; 				 (count-leaves (cdr x))))))

;;; 2.24
;;; *
;;; **
;;; | \
;;; 1  *
;;;   / \
;;;  2   *
;;;     / \
;;;    3   4

;;; 2.25
;;; (car (cdr (car (cdr (cdr pair)))))
;;; (car (car pair))
;;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr pair))))))))))))

;;; 2.26
;;; (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 nil))))))
;;; (cons (con 1 (cons 2 (cons 3 nil))) (cons 4 (cons 5 (cons 6 nil))))
;;; (cons (cons 1 (cons 2 (cons 3 nil))) (cons (cons 4 (cons 5 (cons 6 nil))) nil))

;;; 2.27

(define (deep-reverse list)
  (define (iter r l)
	(cond ((null? l) r)
		  ((not (pair? (car l))) (iter (cons (car l) r) (cdr l)))
		  (else (iter (cons (iter '() (car l)) r) (cdr l)))))
  (iter '() list))

;;; 2.28

(define (fringe x)
  (cond ((null? x) nil)
		((not (pair? (car x))) (cons (car x) (fringe (cdr x))))
		(else (append (fringe (car x)) (fringe (cdr x))))))

;;; 2.29

;; (define (make-mobile left right)
;;   (list left right))

;; (define (make-branch length structure)
;;   (list length structure))
(define (make-mobile left right) (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

;; (define (right-branch mobile)
;;   (car (cdr mobile)))
(define (right-branch mobile)
  (cdr mobile))

(define (mobile? mobile)
  (and (pair? mobile)
	   (branch? (left-branch mobile))
	   (branch? (right-branch mobile))))

(define (branch-length branch)
  (car branch))

;; (define (branch-structure branch)
;;   (car (cdr branch)))
(define (branch-structure branch)
  (cdr branch))

(define (branch? branch)
  (and (pair? branch)
	   (number? (branch-length branch))))

(define (total-weight mobile)
  (define (iter weight tree)
	(cond ((null? tree) weight)
		  ((mobile? tree) (iter (+ weight
								   (iter 0 (left-branch tree))
								   (iter 0 (right-branch tree))) nil))
		  (else (let ((struct (branch-structure tree)))
				  (if (number? struct)
					  (iter (+ weight struct) nil)
					  (iter weight struct))))))
  (iter 0 mobile))

;; (define x (make-mobile (make-branch 3
;; 									(make-mobile (make-branch 6 3)
;; 												 (make-branch 2 9)))
;; 					   (make-branch 3
;; 									(make-mobile (make-branch 2 9)
;; 												 (make-branch 6 3)))))

(define (balance? mobile)
  (define (branch-weight branch)
	(let ((struct (branch-structure branch)))
	  (if (number? struct)
		  struct
		  (total-weight struct))))
  (define (branch-balance? branch)
	(let ((struct (branch-structure branch)))
	  (if (number? struct)
		  true
		  (balance? struct))))
  (let ((left (left-branch mobile))
		(right (right-branch mobile)))
	(and (= (* (branch-length left) (branch-weight left))
			(* (branch-length right) (branch-weight right)))
		 (branch-balance? left)
		 (branch-balance? right))))

;;; d. We only need to change two selectors

;;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
		((not (pair? tree)) (* tree factor))
		(else (cons (scale-tree (car tree) factor)
					(scale-tree (cdr tree) factor)))))

;; (define (scale-tree tree factor)
;;   (map (lambda (sub-tree)
;; 		 (if (pair? sub-tree)
;; 			 (scale-tree sub-tree factor)
;; 			 (* sub-tree factor)))
;; 	   tree))

;;; 2.30

;; (define (square-tree tree)
;;   (cond ((null? tree) nil)
;; 		((not (pair? tree)) (square tree))
;; 		(else (cons (square-tree (car tree))
;; 					(square-tree (cdr tree))))))

;; (define (square-tree tree)
;;   (map (lambda (sub-tree)
;; 		 (if (pair? sub-tree)
;; 			 (square-tree sub-tree)
;; 			 (square sub-tree)))
;; 	   tree))

;;; 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (tree-map proc sub-tree)
			 (proc sub-tree)))
	   tree))

(define (square-tree tree) (tree-map square tree))

;;; 2.32

(define (subsets s)
  (if (null? s)
	  (list nil)
	  (let ((rest (subsets (cdr s))))
		(append rest (map
					  (lambda (x) (cons (car s) x))
					  rest)))))

;;; Sequences as Conventional Interfaces

;; (define (sum-odd-squares tree)
;;   (cond ((null? tree) 0)
;; 		((not (pair? tree))
;; 		 (if (odd? tree) (square tree) 0))
;; 		(else (+ (sum-odd-squares (car tree))
;; 				 (sum-odd-squares (cdr tree))))))

;; (define (even-fibs n)
;;   (define (next k)
;; 	(if (> k n)
;; 		nil
;; 		(let ((f (fib k)))
;; 		  (if (even? f)
;; 			  (cons f (next (+ k 1)))
;; 			  (next (+ k 1))))))
;;   (next 0))

;;; Sequence Operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	  nil
	  (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   nil
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;; (define (salary-of-highest-paid-programmer records)
;;   (accumulate max 0 (map salary (filter programmer? records))))

;;; 2.33

;; (define (map p sequence)
;;   (accumulate (lambda (x y)
;; 				(cons (p x) y) ) nil sequence))
(define (append seq1 seq2)
  (accumulate cons
			  seq2 seq1))
(define (length sequence)
  (accumulate
   (lambda (x y) (inc y)) 0 sequence))

;;; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x)
												   this-coeff))
			  0
			  coefficient-sequence))

;;; 2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
					   (enumerate-tree t))))

;;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  nil
	  (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
			(accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

;;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(map (lambda (row) (matrix-*-vector cols row)) m)))

;;; 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
		result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter initial sequence))

;; (fold-right / 1 (list 1 2 3)) -> 3/2
;; (fold-left / 1 (list 1 2 3)) -> 1/6
;; (fold-right list nil (list 1 2 3)) -> '(1 (2 (3 nil)))
;; (fold-left list nil (list 1 2 3)) -> '(((nil 1) 2) 3)

;;; op must be produce the same result no matter the order of its args to guarantee
;;; that fold-right and fold-left will produce the same values for any sequence

;;; 2.39

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) (append y (list x))) nil sequence))
;; (define (reverse sequence)
;;   (fold-left (lambda (x y) (cons y x)) nil sequence))

;;; Nested Mappings

;; (accumulate
;;  append nil (map (lambda (i)
;; 				   (map (lambda (j) (list i j))
;; 						(enumerate-interval 1 (- i 1))))
;; 				 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;; 	   (filter prime-sum? (flatmap
;; 						   (lambda (i)
;; 							 (map (lambda (j) (list i j))
;; 								  (enumerate-interval 1 (- i 1))))
;; 						   (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

;;; 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
			 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
			 (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum? (unique-pairs n))))

;;; 2.41

(define (unique-triplets n)
  (flatmap (lambda (i)
			 (flatmap (lambda (j)
						(map (lambda (k) (list i j k))
							 (enumerate-interval 1 (- j 1))))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (sum-triplets n s)
  (filter (lambda (t) (= s (+ (car t) (cadr t) (caddr t))))
		  (unique-triplets n)))

;;; 2.42

(define (queens board-size)
  (define empty-board nil)
  (define (adjoin-position new-row k rest-of-queens)
	(cons (list k new-row) rest-of-queens))
  (define (safe? ck positions)
  	(let ((rk (cadr (car positions))))
  	  (accumulate (lambda (ith s?)
  					(let ((ci (car ith))
  						  (ri (cadr ith)))
  					  (and s?
						   (not (= ri rk))
						   (not (= (- ck ci)
								   (abs (- rk ri)))))))
  				  true
  				  (cdr positions))))
  (define (queen-cols k)
	(if (= k 0)
		(list empty-board)
		(filter
		 (lambda (positions) (safe? k positions))
		 (flatmap
		  (lambda (rest-of-queens)
			(map (lambda (new-row)
				   (adjoin-position
					new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		  (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; 2.43
;;; It will run suuuuuuuuuuuper slow because it will re-compute (queens-cols)
;;; each time it enumerate through the list.
;;; So if 2.42 run with T time, 2.43 will run with (* T (fatorial board-size))

;;; A Picture Language

;; (define (flipped-pairs painter)
;;   (let ((painter2 (beside painter (flip-vert painter))))
;; 	(below painter2 painter2)))

;; (define (right-split painter n)
;;    (if (= n 0)
;; 	   painter
;; 	   (let ((smaller (right-split painter (- n 1))))
;; 		 (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
	  painter
	  (let ((up (up-split painter (- n 1)))
			(right (right-split painter (- n 1))))
		(let ((top-left (beside up up))
			  (bottom-right (below right right))
			  (corner (corner-split painter (- n 1))))
		  (beside (below painter top-left)
				  (below bottom-right corner))))))

;; (define (square-limit painter n)
;;   (let ((quarter (corner-split painter n)))
;; 	(let ((half (beside (flip-horiz quarter) quarter)))
;; 	  (below (flip-vert half) half))))

;;; 2.44

;; (define (up-split painter n)
;;   (if (= n 0)
;; 	  painter
;; 	  (let ((smaller (up-split painter (- n 1))))
;; 		(below painter (beside smaller smaller)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
	(let ((top (beside (tl painter) (tr painter)))
		  (bottom (beside (bl painter) (br painter))))
	  (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
								  identity flip-vert)))
	(combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
								  rotate180 flip-vert)))
	(combine4 (corner-split painter n))))

;;; 2.45

(define (split sb sm)
  (define (iter painter n)
	(if (= n 0)
		painter
		(let ((smaller (iter painter (- n 1))))
		  (sb painter (sm smaller smaller)))))
  iter)

(define right-split (split beside below))
(define up-split (split below beside))

;;; 2.46

;; (define make-vect cons)
;; (define xcor-vect car)
;; (define ycor-vect cdr)

;; (define (vector-add v1 v2)
;;   (make-vect (+ (xcor-vect v1) (xcor-vect v2))
;; 			 (+ (ycor-vect v2) (ycor-vect v2))))

;; (define (vector-sub v1 v2)
;;   (vector-add v1 (make-vect (- (xcor-vect v2))
;; 						  (- (ycor-vect v2)))))

;; (define (vector-scale s v)
;;   (make-vect (* s (xcor-vect v))
;; 			 (* s (ycor-vect v))))

;;; 2.47

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; (define origin-frame car)
;; (define edge1-frame cadr)
;; (define edge2-frame caddr)

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; (define frame-origin car)
;; (define (frame-edge1 f) (car (car f)))
;; (define (frame-edge2 f) (car (cdr f)))

;;; Frames

(define (frame-coord-map frame)
  (lambda (v)
	(vector-add
	 (frame-origin frame)
	 (vector-add (vector-scale (vector-xcor v) (frame-edge1 frame))
				 (vector-scale (vector-ycor v) (frame-edge2 frame))))))

;;; Painters

;; (define (segments->painter segment-list)
;;   (lambda (frame)
;; 	(for-each
;; 	 (lambda (segment)
;; 	   (draw-line
;; 		((frame-coord-map frame)
;; 		 (start-segment segment))
;; 		((frame-coord-map frame)
;; 		 (end-segment segment))))
;; 	 segment-list)))

;;; 2.48

(define (make-segment start end) (cons start end))
(define segment-start car)
(define segment-end cdr)

;;; 2.49

(define outline-segments
  (list (make-segment (make-vect 0 0)
					  (make-vect 0 1))
		(make-segment (make-vect 0 0)
					  (make-vect 1 0))
		(make-segment (make-vect 0 1)
					  (make-vect 1 1))
		(make-segment (make-vect 1 0)
					  (make-vect 1 1))))

(define outline (segments->painter outline-segments))

(define x-segments
  (list (make-segment (make-vect 0 1)
					  (make-vect 1 0))
		(make-segment (make-vect 0 0)
					  (make-vect 1 1))))

(define x-painter (segments->painter x-segments))

(define diamond-segments
   (list (make-segment (make-vect 0 0.5)
					   (make-vect 0.5 1))
		 (make-segment (make-vect 0.5 1)
					   (make-vect 1 0.5))
		 (make-segment (make-vect 1 0.5)
					   (make-vect 0.5 0))
		 (make-segment (make-vect 0.5 0)
					   (make-vect 0 0.5))))

(define diamond (segments->painter diamond-segments))

;;; Sameless copy :p. I'm bad at figuring out cordinates
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))

;;; I'm bored so here is my name in picture language :)
(define myname-segments
  (list (make-segment (make-vect 1/16 2/8)
					  (make-vect 1/16 6/8))
		(make-segment (make-vect 2/8 3/8)
					  (make-vect 2/8 5/8))
		(make-segment (make-vect 1/16 2/8)
					  (make-vect 2/8 3/8))
		(make-segment (make-vect 1/16 6/8)
					  (make-vect 2/8 5/8))
		(make-segment (make-vect 5/16 2/8)
					  (make-vect 1/2 6/8))
		(make-segment (make-vect 1/2 6/8)
					  (make-vect 11/16 2/8))
		(make-segment (make-vect 13/32 1/2)
					  (make-vect 19/32 1/2))
		(make-segment (make-vect 23/32 2/8)
					  (make-vect 23/32 6/8))
		(make-segment (make-vect 31/32 2/8)
					  (make-vect 31/32 6/8))
		(make-segment (make-vect 23/32 6/8)
					  (make-vect 31/32 2/8))))

(define myname (segments->painter myname-segments))

;;; Transforming and combining painters

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
	(let ((m (frame-coord-map frame)))
	  (let ((new-origin (m origin)))
		(painter (make-frame
				  new-origin
				  (vector-sub (m corner1) new-origin)
				  (vector-sub (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
					 (make-vect 0.0 1.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
					 (make-vect 0.0 0.0)
					 (make-vect 0.65 0.35)
					 (make-vect 0.35 0.65)))

(define (beside-painter painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
	(let ((paint-left
		   (transform-painter
			painter1
			(make-vect 0.0 0.0)
			split-point
			(make-vect 0.0 1.0)))
		  (paint-right
		   (transform-painter
			painter2
			split-point
			(make-vect 1.0 0.0)
			(make-vect 0.5 1.0))))
	  (lambda (frame)
		(paint-left frame)
		(paint-right frame)))))

;;; 2.50

(define (flip-horiz painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 1.0)))

;;; 2.51

;; (define (below-painter painter1 painter2)
;;   (let ((split-point (make-vect 0.0 0.5)))
;; 	(let ((paint-up
;; 		   (transform-painter
;; 			painter1
;; 			(make-vect 0.0 0.0)
;; 			(make-vect 1.0 0.0)
;; 			split-point))
;; 		  (paint-down
;; 		   (transform-painter
;; 			painter2
;; 			split-point
;; 			(make-vect 1.0 0.5)
;; 			(make-vect 0.0 1.0))))
;; 	  (lambda (frame)
;; 		(paint-up frame)
;; 		(paint-down frame)))))

(define (below-painter painter1 painter2)
  (rotate90 (beside-painter (rotate270 einstein)
							(rotate270 myname))))

;;; Levels of language for robust design

;;; 2.52

;;; 2.52a

(define smile-wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.430 0.875)
   (make-vect 0.480 0.875))
  (make-segment
   (make-vect 0.530 0.875)
   (make-vect 0.580 0.875))
  (make-segment
   (make-vect 0.460 0.75)
   (make-vect 0.540 0.75))
  (make-segment
   (make-vect 0.460 0.75)
   (make-vect 0.400 0.80))
  (make-segment
   (make-vect 0.540 0.75)
   (make-vect 0.600 0.80))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define smile-wave (segments->painter smile-wave-segments))

;;; 2.52b

(define (corner-split-mod painter n)
  (if (= n 0)
	  painter
	  (let ((top-left (up-split painter (- n 1)))
			(bottom-right (right-split painter (- n 1)))
			(corner (corner-split painter (- n 1))))
		(beside (below painter top-left)
				(below bottom-right corner)))))

;;; 2.52c

(define (square-limit-mod painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
								  identity flip-horiz)))
	(combine4 (corner-split painter n))))

;;; Symbolic Data

;;; Quotation

;;; 2.53

;; (list 'a 'b 'c) -> (a b c)
;; (list (list 'george)) -> ((george))
;; (cdr '((x1 x2) (y1 y2))) -> ((y1 y2))
;; (cadr '((x1 x2) (y1 y2))) -> (y1 y2)
;; (pair? (car '(a short list))) -> #f
;; (memq 'red '((red shoes) (blue socks))) -> #f
;; (memq 'red '(red shoes blue socks)) -> (red shoes blue socks)

;;; 2.54

;; (define (equal? a b)
;;   (cond ((and (pair? a) (pair? b)) (and (equal? (car a)
;; 												(car b))
;; 										(equal? (cdr a)
;; 												(cdr b))))
;; 		((or (pair? a) (pair? b)) false)
;; 		(else (eq? a b))))

;;; 2.55
;;; (car ''abracadabra)
;;; (car (quote (quote abracadabra)))
;;; (car '(quote abracadabra))

;;; Example: Symbolic Differentiation

;;; The differentiation program with abstract data

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;; 		((variable? exp) (if (same-variable? exp var) 1 0))
;; 		((sum? exp) (make-sum (deriv (addend exp) var)
;; 							  (deriv (augend exp) var)))
;; 		((product? exp)
;; 		 (make-sum
;; 		  (make-product (multiplier exp)
;; 						(deriv (multiplicand exp) var))
;; 		  (make-product (deriv (multiplier exp) var)
;; 						(multiplicand exp))))
;; 		(else
;; 		 (display "unknown expression type: DERIV" exp))))

;;; Representing algebraic expressions

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; (define (make-sum a1 a2) (list '+ a1 a2))
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;; 		((=number? a2 0) a1)
;; 		((and (number? a1) (number? a2))
;; 		 (+ a1 a2))
;; 		(else (list '+ a1 a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
;; (define (make-product m1 m2) (list '* m1 m2))
;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;; 		((=number? m1 1) m2)
;; 		((=number? m2 1) m1)
;; 		((and (number? m1) (number? m2)) (* m1 m2))
;; 		(else (list '* m1 m2))))
;; (define (sum? x) (and (pair? x) (eq? (car x) '+)))
;; (define (addend s) (cadr s))
;; (define (augend s) (caddr s))
;; (define (product? x) (and (pair? x) (eq? (car x) '*)))
;; (define (multiplier p) (cadr p))
;; (define (multiplicand p) (caddr p))

;;; 2.56

(define (make-exponentiation u n)
  (cond ((=number? n 0) 1)
		((=number? n 1) u)
		(else (list '** u n))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		((sum? exp) (make-sum (deriv (addend exp) var)
							  (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
		((exponentiation? exp)
		 (make-product (make-product
						(exponent exp)
						(make-exponentiation (base exp)
											 (make-sum (exponent exp)
													   -1)))
					   (deriv (base exp) var)))
		(else
		 (display "unknown expression type: DERIV" exp))))

;;; 2.57

;; (define (augend s)
;;   (define (sum a)
;; 	(if (= (length a) 2)
;; 		(list '+ (car a) (cadr a))
;; 		(make-sum (car a) (sum (cdr a)))))
;;   (if (= (length (cddr s)) 1)
;; 	  (caddr s)
;; 	  (sum (cddr s))))
;; (define (multiplicand p)
;;   (define (product m)
;; 	(if (= (length m) 2)
;; 		(list '* (car m) (cadr m))
;; 		(make-product (car m) (product (cdr m)))))
;;   (if (= (length (cddr p)) 1)
;; 	  (caddr p)
;; 	  (product (cddr p))))

;;; 2.58

;;; 2.58a

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2))
		 (+ a1 a2))
		(else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list m1 '* m2))))
;; (define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
;; (define (addend s) (car s))
;; (define (product? x) (and (pair? x) (eq? (cadr x) '*)))
;; (define (multiplier p) (car p))

;;; 2.58

(define (sum? exp) (eq? '+ (min-op exp)))
(define (product? exp) (eq? '* (min-op exp)))
(define (min-op exp)
  (accumulate (lambda (a b)
				(if (operator? a)
					(smaller-op a b)
					b))
			  nil
			  exp))
(define (operator? x)
  (or (eq? '* x) (eq? '+ x) (null? x)))
(define (smaller-op a b)
  (cond ((eq? a b) a)
		((or (eq? '+ a) (eq? '+ b)) '+)
		((eq? a nil) b)
		(else a)))
(define (singleton? a) (= (length a) 1))
(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
	(if (singleton? a)
		(car a)
		a)))
(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
	  '()
	  (cons (car list) (prefix sym (cdr list)))))
(define (addend expr)
  (let ((a (prefix '+ expr)))
	(if (singleton? a)
		(car a)
		a)))
(define (multiplier expr)
  (let ((m (prefix '* expr)))
	(if (singleton? m)
		(car m)
		m)))
(define (multiplicand expr)
  (let ((m (cdr (memq '* expr))))
	(if (singleton? m)
		(car m)
		m)))

;;; Representing Sets

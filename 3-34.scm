(load "constraint.scm")

(define (squarer a b)
  (multiplier a a b))

;;; Multipler only trigger change when 2 of its varibles has value and it do
;;; not know that the 2 a is the same connector. So when we set b, a will not
;;; change.

;;; Test

(define A (make-connector))
(define B (make-connector))

(squarer A B)

(probe "A" A)
(probe "B" B)

(set-value! B 10 'user) ;; Do not trigger A to change value
(forget-value! B 'user)

(set-value! A 10 'user) ;; B set to 100

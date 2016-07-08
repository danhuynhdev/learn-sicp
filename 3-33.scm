(load "constraint.scm")

(define (averager a b c)
  (let ((m (make-connector))
        (v (make-connector)))
    (adder a b v)
    (multiplier c m v)
    (constant 2 m)
    'ok))

;;; Test

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(probe "A" A)
(probe "B" B)
(probe "Average" C)

(averager A B C)

(set-value! A 4 'user)
(set-value! B 4 'user)
(forget-value! A 'user)
(forget-value! B 'user)

(set-value! A 4 'user)
(set-value! B 6 'user)
(forget-value! A 'user)
(forget-value! B 'user)

(set-value! B 6 'user)
(set-value! C 10 'user)

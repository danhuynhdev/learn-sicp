(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
;;; +-> ['a | -]-> ['b -]-> ['c | -]-+
;;; |                                |
;;; +--------------------------------+

;;; It will create a infinite loop.

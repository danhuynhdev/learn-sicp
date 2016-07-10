;;; (define sum 0) -> 0
;;; (define (accum x) (set! sum (+ x sum)) sum) -> 0
;;; (define seq
;;;   (stream-map accum
;;;               (stream-enumerate-interval 1 20))) -> 1
;;; (define y (stream-filter even? seq)) -> 6
;;; (define z
;;;   (stream-filter (lambda (x) (= (remainder x 5) 0))
;;;                  seq)) -> 10
;;; (stream-ref y 7) -> 136
;;; (display-stream z) -> 210

;;; stream-ref -> 136
;;; display-stream -> (10 15 45 55 105 120 190 210)

;;; Yes it will be difference.
;;; Because if we don't use memo-proc accum will run again on every member of
;;; the list exept the first member each time we enumarate over it.
